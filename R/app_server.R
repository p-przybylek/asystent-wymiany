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
      get_tv_con() # to laduje sie ok. 8 sekund, za kazdym razem, gdy urzytkownik wejdzie w TV
    else
      NA
  })
  
  
  output$box_models <- renderUI(
    sidebarLayout(
      sidebarPanel(
        h1(ifelse(urzadzenie() == "fridges","LODÓWKI", "TELEWIZORY"), align="center"),
        fluidRow(selectInput("sorting", NULL, 
                             choices = c("Najtańsze wymiany", "Najbardziej opłacalne wymiany", "Najbardziej energooszczędne wymiany"))),
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
            shinycssloaders::withSpinner(plotOutput("modelplot", height = "660px"), color = "#ffc34d"),
            actionButton("rtmain", "Zobacz inne urządzenia")),
        div(id="box-rightsidebar",
            "Parametry",
            shinycssloaders::withSpinner(uiOutput("image"), color = "#ff8000"),
            shinycssloaders::withSpinner(tableOutput("parameters"), color = "#ff8000"),
            uiOutput("buy"))),
        width=9)
      ))
  
    observe({
      if(is.na(urzadzenie_id())){
        output$modelplot <- renderUI({}) #TODO (Napis "Wybierz model")
        output$image <- renderUI({})
        output$parameters <- renderUI({})
      }
    })
    
    observeEvent(input$rtmain,{
      newtab <- switch(input$tabs,
                       "models" = "main",
                       "main" = "models")
      shinydashboard::updateTabItems(session, "tabs", newtab)
      output$modelplot <- renderUI({})
      output$image <- renderUI({})
      output$parameters <- renderTable({})
      output$buy <- renderUI({})
    })
  
    observe({
      lapply(best_models()$input_ID, function(input_id){
        observeEvent(input[[input_id]], {
                     output$modelplot <- {
                       renderPlot(
                         switch (urzadzenie(),
                           "fridges" = yearly_forecast_plot(cur_m_power = cur_m_power,
                                                            new_m_power = best_models()[best_models()$input_ID == input_id,
                                                                                        'Roczne_zuzycie_pradu_kWh'], # Pobor_mocy_tryb_czuwania_W Pobor_mocy_tryb_wlaczenia_W
                                                            new_m_price = best_models()[best_models()$input_ID == input_id,
                                                                                        'Cena'],
                                                            el_cost = el_cost,
                                                            cur_month_power = cur_month_power),
                           "tvs" = {
                             X <- tv_con()
                             
                             X$new <- get_new_tv_con(best_models()[best_models()$input_ID == input_id,
                                                                   "Pobor_mocy_tryb_czuwania_W"],
                                                     best_models()[best_models()$input_ID == input_id,
                                                                   "Pobor_mocy_tryb_wlaczenia_W"],
                                                     X)
                             
                             names(X)[3] <- "old" # kWh -> old
                             
                             monthly_savings_plot(monthly_con = X[,c("Month", "old", "new")],
                                                  el_cost = el_cost,
                                                  years_to_go = best_models()[best_models()$input_ID == input_id,
                                                                              "years_to_go"],
                                                  new_m_price = best_models()[best_models()$input_ID == input_id,
                                                                              "Cena"])
                           }
                         )
                       )
                     }
                     output$image <- {
                       renderUI({
                         tags$img(src = switch(urzadzenie(),
                                               "fridges" = fridges[best_models()[best_models()$input_ID == input_id,"ID"], "Zdj"],
                                               "tvs" = tvs[best_models()[best_models()$input_ID == input_id,"ID"], "Zdj"]),
                                  width= "90%")
                       })}
                     output$parameters <- {
                       param_table <- switch(urzadzenie(),
                                             "fridges" = data.frame(t(fridges[best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))])),
                                             "tvs" = data.frame(t(tvs[best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))])))
                         
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
      urzadzenie()
      
      isolate({
        if(input[["filters"]] == 0 || is.null(input[["filters"]])) # niema jeszcze przycisku do filtrowania, lub nigdy nie zostal klikniety - dopiero weszlismy do panelu 2
          return(NA)
        
      
      output$modelplot <- renderUI({})
      output$image <- renderUI({})
      output$parameters <- renderTable({})
      output$buy <- renderUI({})  
      
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
    
    output$id_offers <- renderUI({
        all_offers <- get_offers(urzadzenie_id(), urzadzenie())
        utils::data(list = "shops_logo", package = 'asystentWymiany', envir = rlang::current_env())
        fluidRow(column(12, offset = (12-round(12/length(all_offers))*length(all_offers))/2,
                                                 align = "center",
                                                 lapply(all_offers, function(list){
                                                   shinydashboard::box(width = round(12/length(all_offers)), 
                                                                       class = "box",
                                                                       tags$img(src=shops_logo[shops_logo$Sklep == list$Sklep, "Logo"], width="90%", height="100px"),
                                                                       br(),
                                                                       p(paste0(list$Cena, " zł"), class = "text_price"),
                                                                       tags$a(href=list$URL,
                                                                              actionButton(paste("shop_", list$NR), "Przejdź", class = "shops"),
                                                                              target="_blank"))
                                                 })))})
    
    observeEvent(input$rtmodels,{
      newtab <- switch(input$tabs,
                       "offers" = "models",
                       "models" = "offers")
      shinydashboard::updateTabItems(session, "tabs", newtab)
    })
}