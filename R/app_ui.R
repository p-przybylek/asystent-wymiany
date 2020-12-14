#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @export
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(), # Leave this function for adding external resources
    shinydashboard::dashboardPage(title = "Asystent Wymiany",
                                  shinydashboard::dashboardHeader(title = "ASYSTENT WYMIANY SPRZĘTU RTV I AGD",
                                                                  titleWidth = 600,
                                                                  shinydashboard::dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("cog")),
                                                                  shinydashboard::dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("sign-out-alt"))),
                                  shinydashboard::dashboardSidebar(disable = TRUE,
                                                                   shinydashboard::sidebarMenu(id = "tabs",
                                                                                               shinydashboard::menuItem("Strona startowa", tabName = "start"),
                                                                                               shinydashboard::menuItem("Strona główna", tabName = "main"),
                                                                                               shinydashboard::menuItem("Modele", tabName = "models"),
                                                                                               shinydashboard::menuItem("Oferty", tabName = "offers"))),
                                  shinydashboard::dashboardBody(
                                    shinydashboard::tabItems(
                                      shinydashboard::tabItem(tabName = "start",
                                                              fluidRow(column(12, align = "center", actionButton("start_button", "START")))),
                                      shinydashboard::tabItem(tabName = "main",
                                                              fluidRow(column(12, align = "center",
                                                                              selectInput("sorting1", NULL,
                                                                                          choices = c("Najbardziej opłacalne wymiany" = "years_to_go",
                                                                                                      "Najtańsze wymiany" = "prize",
                                                                                                      "Najbardziej energooszczędne wymiany" = "power_efficiency")))),
                                                              div(id = "box-page",
                                                                  uiOutput("kafelki"))),
                                      shinydashboard::tabItem(tabName = "models", uiOutput("box_models")),
                                      shinydashboard::tabItem(tabName = "offers", uiOutput("box_offers"))))),
    tags[["footer"]]("Naatu Energy", class = "footer")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'asystentWymiany'
    ),
    shinyjs::useShinyjs(),       # do reagowania na eventy itp
    shinyalert::useShinyalert() # do wyswietlania bledow itp
  )
}

