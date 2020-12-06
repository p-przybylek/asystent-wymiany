#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @export
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  urzadzenie <- reactiveVal(NA)
  
  shinyjs::onclick("box-fridge", { # user wybral lodowki
    newtab <- switch(input$tabs,
                     "main" = "models",
                     "models" = "main")
    shinydashboard::updateTabItems(session, "tabs", newtab)
    urzadzenie("fridges")
  })
  
  shinyjs::onclick("box-tv", { # user wybral TV
    newtab <- switch(input$tabs,
                     "main" = "models",
                     "models" = "main")
    shinydashboard::updateTabItems(session, "tabs", newtab)
    urzadzenie("tvs")
  })
 
  cur_month_power <- get_fridge_con()
  cur_m_power <- sum(cur_month_power$kWh)
  el_cost <- 0.617 # koszt 1 kWh w złotówkach
  best_models <- reactive({
    out <- get_best_models(urzadzenie(), cur_m_power, el_cost, tv_con = tv_con(), filters = filters())
    if(is.null(out)) return(NULL)
    out$input_ID <- paste0('actionButton_', out[['ID']])
    out$label <- sub(ifelse(urzadzenie() == "fridges", "Lodówka ", "Telewizor "), "", out[['Nazwa']])
    out$label <- sub(" ", "<br>", out[['label']])
    out
  })
  # Replace the second ' ' with <br>
  # spaces <- stringi::stri_locate_all(best_models()$label, fixed = ' ')[[1]]['start']
  
  tv_con <- reactive({
    if(!is.na(urzadzenie()) & urzadzenie() == "tvs")
      get_tv_con()
    else
      NA
  })
  
  
  output$box_models <- renderUI(
    sidebarLayout(
      sidebarPanel(
        h1("LODÓWKI", align="center"),
        fluidRow(selectInput("sorting", NULL, choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany"))),
        if(is.null(best_models()))  shinyalert::shinyalert("",
                                                            "Nie ma takich modeli!",
                                                            type = "error",
                                                            confirmButtonText = "OK",
                                                            confirmButtonCol = "#66cdaa")
        else lapply(best_models()$input_ID, function(id){
                      fluidRow(actionButton(inputId = id,
                                label = HTML(best_models()[best_models()$input_ID == id,'label']),
                                class = 'btn model_input_btn'))
        }),
        fluidRow(column(12, align = "center", actionButton(inputId = "filters",
                                                           label = " + Filtry",
                                                           class = "btn filter"))),
        conditionalPanel("input.filters%2==1",
                         create_filters_elements(get_attr_info(urzadzenie())),
                         fluidRow(column(12, align = "center", actionButton(inputId = "filtering",
                                                                            label = "Zastosuj",
                                                                            class = "btn filtering")))),
        width = 3),
      
      mainPanel(div(
        div(id="box-modelplot",
            " Porównanie zużycia energii ",
            plotOutput("modelplot", height = "660px"),
            actionButton("rtmain", "Zobacz inne urządzenia")),
        div(id="box-rightsidebar",
            "Parametry",
            uiOutput("image"),
            tableOutput("parameters"),
            uiOutput("buy"))),
        width=9)
      ))
  
    observeEvent(input$rtmain,{
      newtab <- switch(input$tabs,
                       "models" = "main",
                       "main" = "models")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
  
    observe({
      lapply(best_models()$input_ID, function(input_id){
        observeEvent(input[[input_id]], {
                     output$modelplot <- {
                       out <- NA
                       if(urzadzenie() == "fridges")
                         out <- yearly_forecast_plot(cur_m_power = cur_m_power,
                                                     new_m_power = best_models()[best_models()$input_ID == input_id,
                                                                                'Roczne_zuzycie_pradu_kWh'], # Pobor_mocy_tryb_czuwania_W Pobor_mocy_tryb_wlaczenia_W
                                                     new_m_price = best_models()[best_models()$input_ID == input_id,
                                                                                'Cena'],
                                                     el_cost = el_cost,
                                                     cur_month_power = cur_month_power)
                       if(urzadzenie() == "tvs"){
                         X <- tv_con()
                       
                         X$new <- get_new_tv_con(best_models()[best_models()$input_ID == input_id,
                                                                "Pobor_mocy_tryb_czuwania_W"],
                                                  best_models()[best_models()$input_ID == input_id,
                                                                "Pobor_mocy_tryb_wlaczenia_W"],
                                                  X)
                         names(X)[3] <- "old" # kWh -> old
                          
                         out <- monthly_savings_plot(monthly_con = X[,c("Month", "old", "new")],
                                                     el_cost = el_cost,
                                                     years_to_go = best_models()[best_models()$input_ID == input_id,
                                                                                 "years_to_go"],
                                                     new_m_price = best_models()[best_models()$input_ID == input_id,
                                                                                 "Cena"])
                       }
                       
                       renderPlot(out)
                     }
                     output$image <- {
                       renderUI({
                         img <- NA
                         if(urzadzenie() == "fridges")
                           img <- fridges[best_models()[best_models()$input_ID == input_id,"ID"], "Zdj"]
                         if(urzadzenie() == "tvs")
                           img <- tvs[best_models()[best_models()$input_ID == input_id,"ID"], "Zdj"]
                         tags$img(src=img, width= "90%")
                       })}
                     output$parameters <- {
                       param_table <- NA
                       if(urzadzenie() == "fridges")
                         param_table <- data.frame(t(fridges[best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))]))
                       if(urzadzenie() == "tvs")
                         param_table <- data.frame(t(tvs[best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))]))
                         
                       rownames(param_table) <- lapply(rownames(param_table), function(rowname){
                         stringi::stri_replace_all_fixed(rowname, "_", " ")
                       })
                       param_table <- tibble::rownames_to_column(param_table, "Parametry")
                       colnames(param_table) <- NULL
                       renderTable(param_table, width = "100%")
                       }
                     output$buy <- {
                       renderUI(actionButton("buybutton","Kup teraz!"))
                     }})
      })
    })
    
    observeEvent(input$buybutton,{
      newtab <- switch(input$tabs,
                       "models" = "offers",
                       "offers" = "models")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
    
    
    filtering <- reactiveVal(0)
    
    shinyjs::onclick("filtering", {  # gdy user zaaplikuje nowe filtry, aktualizuje filtering(), a w konsekwencji filters()
      filtering(filtering() + 1)
    })
    
    filters <- reactive({
      filtering()
      
      isolate({
        if(is.null(input[["filters"]])){ # niema jeszcze filterow - dopiero weszlismy do panelu 2
          return(NA)
        }
      
        lapply(get_attr_info(urzadzenie()), function(list){
          filter_id <- ifelse(identical(list$type, "numeric"),
                              paste0("filter__", list$name, "__slider"),
                              paste0("filter__", list$name, "__list"))
          list$range <- input[[filter_id]]
          if(identical(list$type, "numeric")) names(list$range) <- c("min", "max")
          list
        })
      })
    })
   
    output$box_offers <- renderUI(
      div(fluidRow(column(12, align = "center", h1("Najlepsze oferty dla:"))),
          fluidRow(column(12, align = "center", h1(best_models()[best_models()$ID == urzadzenie_id(),'Nazwa']))),
          br(),
          uiOutput("id_offers"),
          br(),
          fluidRow(column(12, align = "center", actionButton("rtmodels", "Zobacz inne modele"))))
    )
    
    urzadzenie_id <- reactiveVal(NA)
    
    observe({
      lapply(best_models()$input_ID, function(input_id){
        observeEvent(input[[input_id]], {
                      id <- stringr::str_extract(input_id, "\\d+")
                      urzadzenie_id(id)
    })})})
    
    output$id_offers <- renderUI(fluidRow(get_offers(urzadzenie_id(), urzadzenie())))
    
    observeEvent(input$rtmodels,{
      newtab <- switch(input$tabs,
                       "offers" = "models",
                       "models" = "offers")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
}