#' @import shiny
app_ui <- function() {
  tagList(
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
                    tags[["link"]](rel = "stylesheet", type = "text/css", href = "www/styl.css"),
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
    tags[["footer"]]("Naatu Energy", class = "footer"))
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', app_sys('app/www')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/styl.css"), 
    tags$script(src="www/script.js")
  )
}
