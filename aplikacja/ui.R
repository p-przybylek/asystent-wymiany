library(shiny)
library(shinydashboard)

shinyUI(
  tagList(
    dashboardPage(title = "Asystent Wymiany",
                  dashboardHeader(title = "ASYSTENT WYMIANY SPRZĘTU RTV I AGD",
                                  titleWidth = 600,
                                  dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("cog")),
                                  dropdownMenu(type = NULL, badgeStatus = NULL, icon = icon("sign-out-alt"))),
                  dashboardSidebar(disable = TRUE,
                                   sidebarMenu(menuItem("Strona główna", tabName = "main"),
                                               menuItem("Porównanie kosztów zużycia energii", tabName = "models"),
                                               menuItem("Oferty", tabName = "oferts"))),
                  dashboardBody(
                    tags[["link"]](rel = "stylesheet", type = "text/css", href = "styl.css"),
                    tabItems(
                      tabItem(tabName = "main",
                              fluidRow(column(12, align="center", selectInput("sorting", NULL, choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany")))),
                              div(id = "box-page",
                              fluidRow(
                                box(width = 4),
                                box(width = 4),
                                box(width = 4)),
                              fluidRow(
                                box(width = 4),
                                box(width = 4),
                                box(width = 4)))),
                      tabItem(tabName = "models"),
                      tabItem(tabName = "oferts")))),
    tags[["footer"]]("Naatu Energy", class = "footer")))

