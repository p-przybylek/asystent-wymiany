library(shiny)
library(shinydashboard)

shinyUI(
  tagList(
    dashboardPage(title = "Asystent Wymiany",
                  dashboardHeader(title = "ASYSTENT WYMIANY SPRZĘTU RTV I AGD",
                                  titleWidth = 600),
                  dashboardSidebar(disable = TRUE),
                  dashboardBody(
                    tags[["link"]](rel = "stylesheet", type = "text/css", href = "styl.css")
                  )),
    tags[["footer"]]("Naatu Energy", class = "footer")))

