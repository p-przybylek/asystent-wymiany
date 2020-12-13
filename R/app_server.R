#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @export
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  urzadzenie <- reactiveVal(NA)
  urzadzenie_id <- reactiveVal(NA)
  
  shinyjs::onclick("start_button", { # user zaczyna swa wspaniala przygode z aplikacja
    newtab <- switch(input$tabs,
                     "start" = "main",
                     "main" = "start")
    shinydashboard::updateTabItems(session, "tabs", newtab)
  })
  
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
  
  el_cost <- 0.617 # koszt 1 kWh w złotówkach
  
  # lodowka
  cur_month_power_fridge <- get_fridge_con()
  cur_m_power_fridge <- sum(cur_month_power_fridge$kWh)
  
  # TV
  tv_con <- get_tv_con() # to laduje sie ok. 4 sekund, nie zmienia się w czasie dzialania aplikacji. Żeby przeładować dane, trzeba zrestartowac aplikacje.
  
  sorting <- reactiveVal("years_to_go")
  
  # gdy zmiena sie sortowanie, uaktualnij  wybrana wartosc
  observeEvent(input$sorting1, {
    sorting(input$sorting1)
    urzadzenie_id(NA)
  })
  observeEvent(input$sorting2, {
    sorting(input$sorting2)
    urzadzenie_id(NA)
    updateSelectInput(session, "sorting1", selected = sorting())
  })
  
  best_models <- reactive({
    out <- get_best_models(urzadzenie = urzadzenie(), cur_m_power = cur_m_power_fridge, tv_con = tv_con,
                           el_cost = el_cost, filters = filters(), criterion = sorting())
    if(is.null(out)) return(NULL)
    out$input_ID <- paste0('actionButton_', out[['ID']])
    out$label <- sub(ifelse(urzadzenie() == "fridges", "Lodówka ", "Telewizor "), "", out[['Nazwa']])
    out$label <- sub(" ", "<br>", out[['label']])
    out
  })
  # Replace the second ' ' with <br>
  # spaces <- stringi::stri_locate_all(best_models()$label, fixed = ' ')[[1]]['start']
  
  usun_wyswietlany_model <- function(){ # usuwa elementy UI zalezne od wybranego modelu
    output$modelplot  <- renderUI({})
    output$image      <- renderUI({})
    output$parameters <- renderUI({})
    output$buy        <- renderUI({})
  }
  
  output$box_models <- renderUI(
    sidebarLayout(
      sidebarPanel(
        h1(ifelse(urzadzenie() == "fridges", "LODÓWKI", "TELEWIZORY"), align="center"),
        fluidRow(selectInput("sorting2", NULL, c("Najbardziej opłacalne wymiany" = "years_to_go",
                                                 "Najtańsze wymiany" = "prize",
                                                 "Najbardziej energooszczędne wymiany" = "power_efficiency"), 
                             selected = sorting())),
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
                                                           label = "Filtruj",
                                                           class = "btn filter"))),
        width = 3),
      mainPanel(div(
        div(id="box-modelplot",
            " Porównanie zużycia energii ",
            shinycssloaders::withSpinner(plotOutput("modelplot", height = "660px"), color = "#ffc34d"),
            actionButton("rtmain", "Zobacz inne urządzenia")),
        div(id="box-rightsidebar",
            "Parametry",
            shinycssloaders::withSpinner(uiOutput("image"), color = "#ffc34d"),
            shinycssloaders::withSpinner(tableOutput("parameters"), color = "#ff8000"),
            uiOutput("buy"))),
        width=9)
    )
  )
  
  observe({
    if(is.na(urzadzenie_id())){
      usun_wyswietlany_model()
    }
  })
  
  output$kafelki <- renderUI({
    # kolejnosc w ktorej sa posortowane urzadzenia po kryterium
    kolejnosc <- get_kolejnosc_kafelkow_interface1(cur_m_power_fridge = cur_m_power_fridge,
                                                   tv_con = tv_con,
                                                   el_cost = el_cost,
                                                   criterion = sorting())
    do_wyswietlenia <- attr(kolejnosc, "do_wyswietlania") # kryterium sortowania: cena, czas do zwrotu, miesieczna oszczednosc
    
    # tu posortowane kafelki beda ustawiane w odpowiedniej kolejnosci
    div(id = "kafelki-box",
      fluidRow(
        box_interfejs1("box-fridge", "LODÓWKI"),
        box_interfejs1("box-tv", "TELEWIZORY"),
        box_interfejs1("box-kettle", "CZAJNIKI")),
      fluidRow(
        box_interfejs1("box-washing-machine", "PRALKI"),
        box_interfejs1("box-air-conditioning", "KLIMATYZACJE"),
        box_interfejs1("box-microwave", "MIKROFALÓWKI"))
    )
  })
  
  observeEvent(input$filters, {
    showModal(
      modalDialog(
        wellPanel(
          class = "filters",
          create_filters_elements(get_attr_info(urzadzenie()))
        ),
        actionButton(inputId = "filtering", label = "Zastosuj", class = "btn filtering"),
        easyClose = F,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$filtering, {
    removeModal()
  })
  
  observeEvent(input$rtmain, {
    newtab <- switch(input$tabs,
                     "models" = "main",
                     "main" = "models")
    shinydashboard::updateTabItems(session, "tabs", newtab)
    
    usun_wyswietlany_model()
    
    updateSelectInput(session, "sorting2", NULL, c("Najbardziej opłacalne wymiany" = "years_to_go", "Najtańsze wymiany" = "prize", "Najbardziej energooszczędne wymiany" = "power_efficiency"), selected = sorting())
  })
  
  observe({
    lapply(best_models()$input_ID, function(input_id){
      shinyjs::onclick(input_id, {
        id <- stringr::str_extract(input_id, "\\d+")
        urzadzenie_id(id)
        output$modelplot <- {
          renderPlot(
            switch (urzadzenie(),
                    "fridges" = yearly_forecast_plot(cur_m_power = cur_m_power_fridge,
                                                     new_m_power = best_models()[best_models()$input_ID == input_id,
                                                                                 'Roczne_zuzycie_pradu_kWh'], # Pobor_mocy_tryb_czuwania_W Pobor_mocy_tryb_wlaczenia_W
                                                     new_m_price = best_models()[best_models()$input_ID == input_id,
                                                                                 'Cena'],
                                                     el_cost = el_cost,
                                                     cur_month_power = cur_month_power),
                    "tvs" = {
                      X <- tv_con
                      
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
                                  "tvs"     = tvs    [best_models()[best_models()$input_ID == input_id,"ID"], "Zdj"]),
                     width= "90%")
          })}
        output$parameters <- {
          param_table <- switch(urzadzenie(),
                                "fridges" = data.frame(t(fridges[best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))])),
                                "tvs"     = data.frame(t(tvs    [best_models()[best_models()$input_ID == input_id,"ID"], names(get_attr_info(urzadzenie()))])))
          
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
    urzadzenie_id(NA)
    
    isolate({
      if(input[["filters"]] == 0 || is.null(input[["filters"]])) # niema jeszcze przycisku do filtrowania, lub nigdy nie zostal klikniety - dopiero weszlismy do panelu 2
        return(NA)
      
      
      usun_wyswietlany_model()
      
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
