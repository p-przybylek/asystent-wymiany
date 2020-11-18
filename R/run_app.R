options(shiny.maxRequestSize = 10*1024^2) # W celu rozszezenia maksymalnego rozmiaru pliku mozliwego do przetwozenia przez shiny

library(shiny)
library(shinyjs)
library(shinydashboard)

#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function() {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list()
  )
}
