#' @import shiny
#' @import golem
app_server <- function(input, output,session) {
  # List the first level callModules here
  shinyjs::onclick("box-fridge", {
    newtab <- switch(input$tabs,
                     "main" = "models",
                     "models" = "main")
    shinydashboard::updateTabItems(session, "tabs", newtab)
  })
}
