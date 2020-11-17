library(shiny)
library(shinyjs)
library(shinydashboard)

shinyServer(function(input, output, session) {
  onclick("box-fridge", {
    newtab <- switch(input$tabs,
                     "main" = "models",
                     "models" = "main")
    updateTabItems(session, "tabs", newtab)
  })
})
