#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
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
  cur_m_cost <- 500
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
                                                       el_cost = el_cost))
                       })
      })
      })
}

#' Get best fridges
#' 
#' @return A data.frame with 4 columns: ID, Nazwa, Cena, Roczne_zuzycie_pradu_kWh
#' with info about most cost-efficient top_n fridges 
get_best_fridges <- function(cur_m_power, el_cost, top_n=5){
  data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  fridges$years_to_go <- sapply(1:nrow(fridges), function(i){
    get_years_to_go(cur_m_power, 
                    new_m_power = fridges[i,'Roczne_zuzycie_pradu_kWh'],
                    new_m_price = fridges[i,'Cena'],
                    el_cost)
  })
  fridges[order(fridges$years_to_go)[1:top_n],c('ID', "Nazwa", "Cena", "Roczne_zuzycie_pradu_kWh")]
  
}

