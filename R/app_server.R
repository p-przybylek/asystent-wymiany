#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @export
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  shinyjs::onclick("box-fridge", {
    newtab <- switch(input$tabs,
                     "main" = "models",
                     "models" = "main")
    shinydashboard::updateTabItems(session, "tabs", newtab)
  })
 
  # fridges - 5 przykładowych modeli lodówek do testu, na razie wpisane ręcznie, potem pobierane z bazy modeli
  cur_month_power <- get_fridge_con()
  cur_m_cost <- sum(cur_month_power$kWh)
  el_cost <- 0.617
  best_fridges <- get_best_fridges(cur_m_cost, el_cost)
  best_fridges$input_ID <- paste0('actionButton_',best_fridges[['ID']])
  best_fridges$label <- sub("Lodówka ","",best_fridges[['Nazwa']])
  best_fridges$label <- sub(" ","<br>",best_fridges[['label']])
  # Replace the second ' ' with <br>
  # spaces <- stringi::stri_locate_all(best_fridges$label, fixed = ' ')[[1]]['start']
  
  output$box_models <- renderUI(
    sidebarLayout(
      sidebarPanel(
        h1("LODÓWKI", align="center"),
        lapply(best_fridges$input_ID, function(id){
          fluidRow(actionButton(inputId = id,
                                label = HTML(best_fridges[best_fridges$input_ID == id,'label']),
                                class = 'btn model_input_btn'))
        }),
        fluidRow(column(12, align = "center", actionButton(inputId = "filters",
                                                           label = " + Filtry",
                                                           class = "btn filter"))),
        conditionalPanel("input.filters%2==1",
                         create_filters_elements(get_attr_info()),
                         fluidRow(column(12, align = "center", actionButton(inputId = "filtering",
                                                                            label = "Zastosuj",
                                                                            class = "btn filtering")))),
        width = 3),
      mainPanel(div(id="box-modelplot",
                    " Porównanie zużycia energii ", 
                    plotOutput("modelplot",height = "700px")))))
    observe({
      lapply(best_fridges$input_ID, function(input_id){
        observeEvent(input[[input_id]],
                     output$modelplot <- {
                       renderPlot(yearly_forecast_plot(cur_m_power = cur_m_cost,
                                                       new_m_power = best_fridges[best_fridges$input_ID == input_id,
                                                                                  'Roczne_zuzycie_pradu_kWh'],
                                                       new_m_price = best_fridges[best_fridges$input_ID == input_id,
                                                                                  'Cena'],
                                                       el_cost = el_cost,
                                                       cur_month_power = cur_month_power))
                       })
      })
      })
    
    shinyjs::onclick("filtering", {
      filters <- lapply(get_attr_info(), function(list){
                          filter_id <- ifelse(identical(list$type, "numeric"),
                                              paste0("filter__", list$name, "__slider"),
                                              paste0("filter__", list$name, "__list"))
                          list$range <- input[[filter_id]]
                          if(identical(list$type, "numeric")) names(list$range) <- c("min", "max")
                          list
                        })
    })
    
    observeEvent(input[["filters"]],{
      if(input[["filters"]]%%2==0) {
        filters <- NA
      }
    })
}