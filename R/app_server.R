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
  fridges <- data.frame("BEKO"=c(155, 2200), "AMICA"=c(160, 2300), "LG"=c(170, 2000), "SAMSUNG"=c(183,2200), "BOSH"=c(150,3500))
  nms <- names(fridges)
  
    output$box_models <- renderUI(
      sidebarLayout(
        sidebarPanel(
          h1("LODÓWKI", align="center"),
          lapply(nms, function(name){fluidRow(actionButton(name,name))}), width = 2),
        
        mainPanel(div(id="box-modelplot"," Porównanie zużycia energii ", plotOutput("modelplot",height = "700px")))))
    
    
    observe({
      lapply(nms, function(name){
        observeEvent(input[[name]],
                     output$modelplot <- renderPlot(yearly_forecast_plot(500, fridges[1,name],fridges[2,name],0.617)))
      })})
    
    
  
}
