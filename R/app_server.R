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
  best_fridges <- reactive({
    out <- get_best_fridges(cur_m_cost, el_cost, filters = filters())
    if(is.null(out)) return(NULL)
    out$input_ID <- paste0('actionButton_',out[['ID']])
    out$label <- sub("Lodówka ","",out[['Nazwa']])
    out$label <- sub(" ","<br>",out[['label']])
    out
  })
  # Replace the second ' ' with <br>
  # spaces <- stringi::stri_locate_all(best_fridges()$label, fixed = ' ')[[1]]['start']
  
  output$box_models <- renderUI(
    sidebarLayout(
      sidebarPanel(
        h1("LODÓWKI", align="center"),
        fluidRow(selectInput("sorting", NULL, choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany"))),
        if(is.null(best_fridges()))  shinyalert::shinyalert("",
                                                            "Nie ma takich modeli!",
                                                            type = "error",
                                                            confirmButtonText = "OK",
                                                            confirmButtonCol = "#66cdaa")
        else lapply(best_fridges()$input_ID, function(id){
                      fluidRow(actionButton(inputId = id,
                                label = HTML(best_fridges()[best_fridges()$input_ID == id,'label']),
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
      
      mainPanel(div(
        div(id="box-modelplot",
            " Porównanie zużycia energii ",
            plotOutput("modelplot", height = "700px")),
        div(id="box-rightsidebar",
            "Parametry",
            uiOutput("image"),
            tableOutput("parameters"),
            uiOutput("buy"))),
        width=9)
      ))
  
  
    observe({
      lapply(best_fridges()$input_ID, function(input_id){
        observeEvent(input[[input_id]], {
                     output$modelplot <- {
                       renderPlot(yearly_forecast_plot(cur_m_power = cur_m_cost,
                                                       new_m_power = best_fridges()[best_fridges()$input_ID == input_id,
                                                                                  'Roczne_zuzycie_pradu_kWh'],
                                                       new_m_price = best_fridges()[best_fridges()$input_ID == input_id,
                                                                                  'Cena'],
                                                       el_cost = el_cost,
                                                       cur_month_power = cur_month_power))
                     }
                     output$image <- {
                       renderUI({
                         img <- fridges[best_fridges()[best_fridges()$input_ID == input_id,"ID"], "Zdj"]
                         tags$img(src=img, width= "90%")
                       })}
                     output$parameters <- {
                       param_table <- data.frame(t(fridges[best_fridges()[best_fridges()$input_ID == input_id,"ID"], names(get_attr_info())]))
                       rownames(param_table) <- lapply(rownames(param_table), function(rowname){
                         stringi::stri_replace_all_fixed(rowname, "_", " ")
                       })
                       param_table <- tibble::rownames_to_column(param_table, "Parametry")
                       colnames(param_table) <- NULL
                       renderTable(param_table, width = "100%")
                       }
                     output$buy <-{
                       renderUI(actionButton("buybutton","Kup teraz!"))
                     }})
      })
    })
    
    observeEvent(input$buybutton,{
      newtab <- switch(input$tabs,
                       "models" = "offers",
                       "offers" = "models")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
    
    
    filtering <- reactiveVal(0)
    
    shinyjs::onclick("filtering", {
      filtering(filtering() + 1)
    })
    
    filters <- reactive({
      filtering()
      
      
      if(is.null(isolate(input[["filters"]]))){
        return(NA)
      }
      
      
      isolate(
      lapply(get_attr_info(), function(list){
        filter_id <- ifelse(identical(list$type, "numeric"),
                            paste0("filter__", list$name, "__slider"),
                            paste0("filter__", list$name, "__list"))
        list$range <- input[[filter_id]]
        if(identical(list$type, "numeric")) names(list$range) <- c("min", "max")
        list
      }))
    })
  
    output$box_offers <- renderUI(
      div(fluidRow(column(12, align = "center", h1("Najlepsze oferty"))),
      fluidRow(column(12, align = "center", actionButton("rtmodels", "Zobacz inne modele"))))
    )
    
    observeEvent(input$rtmodels,{
      newtab <- switch(input$tabs,
                       "offers" = "models",
                       "models" = "offers")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
}