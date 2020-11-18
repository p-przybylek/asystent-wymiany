#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboard::dashboardPage(title = "Asystent Wymiany",
                                  shinydashboard::dashboardHeader(title = "ASYSTENT WYMIANY SPRZĘTU RTV I AGD",
                                                                  titleWidth = 600,
                                                                  shinydashboard::dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("cog")),
                                                                  shinydashboard::dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("sign-out-alt"))),
                                  shinydashboard::dashboardSidebar(disable = TRUE,
                                                                   shinydashboard::sidebarMenu(id = "tabs",
                                                                                               shinydashboard::menuItem("Strona główna", tabName = "main"),
                                                                                               shinydashboard::menuItem("Porównanie kosztów zużycia energii", tabName = "models"),
                                                                                               shinydashboard::menuItem("Oferty", tabName = "oferts"))),
                                  shinydashboard::dashboardBody(
                                    shinyjs::useShinyjs(),
                                    shinydashboard::tabItems(
                                      shinydashboard::tabItem(tabName = "main",
                                                              fluidRow(column(12, align = "center", selectInput("sorting", NULL, choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany")))),
                                                              div(id = "box-page",
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-fridge",
                                                                                        p("LODÓWKI")),
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-tv",
                                                                                        p("TELEWIZORY")),
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-kettle",
                                                                                        p("CZAJNIKI"))),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-washing-machine",
                                                                                        p("PRALKI")),
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-air-conditioning",
                                                                                        p("KLIMATYZACJE")),
                                                                    shinydashboard::box(width = 4, class = "box", id = "box-microwave",
                                                                                        p("MIKROFALÓWKI"))))),
                                      shinydashboard::tabItem(tabName = "models"),
                                      shinydashboard::tabItem(tabName = "oferts")))),
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

