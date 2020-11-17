library(shiny)
library(shinyjs)
library(shinydashboard)

shinyUI(
  tagList(
    dashboardPage(title = "Asystent Wymiany",
                  dashboardHeader(title = "ASYSTENT WYMIANY SPRZĘTU RTV I AGD",
                                  titleWidth = 600,
                                  dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("cog")),
                                  dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("sign-out-alt"))),
                  dashboardSidebar(disable = TRUE,
                                   sidebarMenu(id = "tabs",
                                               menuItem("Strona główna", tabName = "main"),
                                               menuItem("Porównanie kosztów zużycia energii", tabName = "models"),
                                               menuItem("Oferty", tabName = "oferts"))),
                  dashboardBody(
                    useShinyjs(),
                    tags[["link"]](rel = "stylesheet", type = "text/css", href = "styl.css"),
                    tabItems(
                      tabItem(tabName = "main",
                              fluidRow(column(12, align = "center", selectInput("sorting", NULL, choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany")))),
                              div(id = "box-page",
                              fluidRow(
                                box(width = 4, class = "box", id = "box-fridge",
                                    p("LODÓWKI")),
                                box(width = 4, class = "box", id = "box-tv",
                                    p("TELEWIZORY")),
                                box(width = 4, class = "box", id = "box-kettle",
                                    p("CZAJNIKI"))),
                              fluidRow(
                                box(width = 4, class = "box", id = "box-washing-machine",
                                    p("PRALKI")),
                                box(width = 4, class = "box", id = "box-air-conditioning",
                                    p("KLIMATYZACJE")),
                                box(width = 4, class = "box", id = "box-microwave",
                                    p("MIKROFALÓWKI"))))),
                      tabItem(tabName = "models"),
                      tabItem(tabName = "oferts")))),
    tags[["footer"]]("Naatu Energy", class = "footer")))

