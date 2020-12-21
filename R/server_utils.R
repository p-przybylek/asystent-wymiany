#' Najlepsze urzadzenia
#' 
#' Znajdz i zwroc najlepsze urzadzenia w zaleznosci od parametru `criterion`.
#' 
#' @inheritParams yearly_forecast_plot
#' @param urzadzenie typ urzadzenia do wyswietlenia
#' @param cur_m_power roczne zuzycie pradu przez urzadzenie
#' @param el_cost koszt 1kWh pradu w domostwie
#' @param top_n liczba najleprzych modeli do pokazania
#' @param filters filtry do zaaplikowania (być może NA)
#' @param criterion String, jeden z 4: "years_to_go" (domyślnie), "power_efficiency", "prize", "true_cost".
#' years_to_go - ile czasu czekamy do zwrotu inwestycji
#' power_efficiency - największa różnica w MIESIĘCZNYM zużyciu prądu
#' prize - co najmniej kosztuje sposrod tych, ktore sa bardziej energooszczedne od aktualnego
#' true_cost - cena produktu minus zysk na kosztach pradu w poruwnaniu z aktualnym modelem
#' 
#' @return Patrz `get_best_fridges` oraz `get_best_tvs`
#' @export
#' 
get_best_models <- function(urzadzenie, 
                            cur_m_power_fridge, 
                            tv_con, 
                            el_cost, 
                            top_n = 5, 
                            filters = NA,
                            criterion = "years_to_go"){
  if(is.na(urzadzenie)) # aplikacja dopiero sie odpala
    return(NULL)
  allowed_appliances <- c('fridges', 'tvs')
  if(!is.character(urzadzenie) || 
     length(urzadzenie) > 1 || 
     !urzadzenie %in% allowed_appliances){
    rlang::abort(paste0('`urzadzenie` must one of: ', stringr::str_flatten(allowed_appliances, collapse = ', ')))
  }
  allowed_criterions <- c("power_efficiency", "prize", "years_to_go", "true_cost")
  if(!is.character(criterion) || 
     length(criterion) > 1 || 
     !criterion %in% allowed_criterions){
    rlang::abort(paste0('`criterion` must one of: ', stringr::str_flatten(allowed_criterions, collapse = ', ')))
  }
  return(switch(urzadzenie, # odpala odpowiednia funkcje dla danego urzadzenia
                "fridges" = get_best_fridges(cur_m_power_fridge, el_cost, top_n, filters, criterion),
                "tvs"     = get_best_tvs    (tv_con,             el_cost, top_n, filters, criterion)))
}


#' Najlepsze lodowki
#' 
#' Znajdz najlepsze lodowki wedlug `criterion`.
#' 
#' @inheritParams get_best_models
#' 
#' @return `data.frame` z kolumnami: ID, Nazwa, Cena, Roczne_zuzycie_pradu_kWh, criterion
#' z informacjami o najlepszych lodowkach wg parametru `criterion`.
#' @export
#' 
get_best_fridges <- function(cur_m_power, el_cost, top_n = 5, filters = NA, criterion){
  utils::data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  if(!sprawdz_poprawnosc_lodowek(fridges))
    shinyalert::shinyalert("",
                           "Dane w pliku fridges są niepoprawne. Zgłoś błąd administratorowi.",
                           type = "error",
                           confirmButtonText = "OK",
                           confirmButtonCol = "#66cdaa")
  fridges <- filter_by_attr(filters = filters, dataset = fridges)
  # Istotne dla `prize`, w pozostałych nie robi różnicy
  fridges <- fridges[fridges$Roczne_zuzycie_pradu_kWh < cur_m_power,]
  if(nrow(fridges) == 0) return(NULL) # gdy filtrowanie sprawilo, ze nic nie zostalo
  fridges$years_to_go <- sapply(1:nrow(fridges), function(i){
    get_years_to_go(cur_m_power, 
                    new_m_power = fridges[i,'Roczne_zuzycie_pradu_kWh'],
                    new_m_price = fridges[i,'Cena'],
                    el_cost)
  })
  fridges <- fridges[fridges$years_to_go < 100, ] # liczba lat do zwrotu mniejsza niz 100
  if(nrow(fridges) == 0) return(NULL) # gdy filtrowanie sprawilo, ze nic nie zostalo
  fridges$Miesieczna_roznica_zuzycia_pradu <- (cur_m_power - fridges$Roczne_zuzycie_pradu_kWh)/12
  fridges$Bilans_po_5_latach <- sapply(1:nrow(fridges), function(i){
    get_true_cost(cur_m_power, 
                    new_m_power = fridges[i,'Roczne_zuzycie_pradu_kWh'],
                    new_m_price = fridges[i,'Cena'],
                    el_cost)
  })
  criterion_column <- switch (criterion,
    'years_to_go'      = 'years_to_go',
    'prize'            = 'Cena',
    'power_efficiency' = 'Miesieczna_roznica_zuzycia_pradu',
    'true_cost'        = 'Bilans_po_5_latach'
  )
  fridges$criterion <- fridges[[criterion_column]]
  fridges[head(order(fridges[['criterion']], decreasing = criterion %in% c('power_efficiency')), n=top_n),c('ID', "Nazwa", "Cena", "Roczne_zuzycie_pradu_kWh", 'criterion')]
}

#' Najlepsze telewizory
#' 
#' Znajdz najlepsze telewizory wedlug `criterion`.
#' 
#' @inheritParams get_best_models
#' 
#' @return `data.frame` z kolumnami: ID, Nazwa, Cena, Roczne_zuzycie_pradu_kWh,Pobor_mocy_tryb_czuwania_W, Pobor_mocy_tryb_wlaczenia_W, years_to_go, criterion
#' z informacjami o najlepszych telewizorach wg parametru `criterion`.
#' @export
#' 
get_best_tvs <- function(tv_con, el_cost, top_n = 5, filters = NA, criterion){
  cur_m_power <- sum(tv_con$kWh)
  
  utils::data('tvs', package = 'asystentWymiany', envir = rlang::current_env())
  if(!sprawdz_poprawnosc_tv(tvs))
    shinyalert::shinyalert("",
                           "Dane w pliku tvs są niepoprawne. Zgłoś błąd administratorowi.",
                           type = "error",
                           confirmButtonText = "OK",
                           confirmButtonCol = "#66cdaa")
  tvs <- filter_by_attr(filters = filters, dataset = tvs)
  if(nrow(tvs) == 0) return(NULL) # gdy filtrowanie sprawilo, ze nic nie zostalo
  tvs$Pobor_mocy_est <- sapply(1:nrow(tvs), function(i)
    sum(get_new_tv_con(tvs[i,"Pobor_mocy_tryb_czuwania_W"],
                       tvs[i,"Pobor_mocy_tryb_wlaczenia_W"],
                       tv_con))
  )
  # Istotne dla `prize`, w pozostałych nie robi różnicy
  tvs <- tvs[tvs$Pobor_mocy_est < cur_m_power,]
  if(nrow(tvs) == 0) return(NULL)
  tvs$years_to_go <- sapply(1:nrow(tvs), function(i){
    get_years_to_go(cur_m_power,
                    new_m_power = tvs[i,'Pobor_mocy_est'],
                    new_m_price = tvs[i,'Cena'],
                    el_cost)
  })
  tvs$Bilans_po_5_latach <- sapply(1:nrow(tvs), function(i){
    get_true_cost(cur_m_power,
                    new_m_power = tvs[i,'Pobor_mocy_est'],
                    new_m_price = tvs[i,'Cena'],
                    el_cost)
  })
  tvs$Miesieczna_roznica_zuzycia_pradu <- (cur_m_power - tvs$Pobor_mocy_est)/12
  criterion_column <- switch (criterion,
                              'years_to_go'      = 'years_to_go',
                              'prize'            = 'Cena',
                              'power_efficiency' = 'Miesieczna_roznica_zuzycia_pradu',
                              'true_cost'        = 'Bilans_po_5_latach'
  )
  tvs$criterion <- tvs[[criterion_column]]
  tvs[head(order(tvs[['criterion']], decreasing = (criterion %in% c('power_efficiency'))), n=top_n), c('ID', "Nazwa", "Cena", "Pobor_mocy_tryb_czuwania_W", "Pobor_mocy_tryb_wlaczenia_W", "years_to_go", 'criterion')]
}

#' Policz opłacalność
#' 
#' @inheritParams yearly_forecast_plot
#' @describeIn get_years_to_go Policz czas do zwrotu inwestycji
#' @return ilość lat, po których zakup się zwróci (`get_years_to_go`) lub bilans po 5 latach (`get_true_cost`)
#' 
#' @export

#' @examples
#' cur_m_power <- 400 # kWh/rok
#' new_m_power <- 170 # kWh/rok
#' new_m_price <- 1200 # PLN
#' el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)
#' test_plot <- yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost, get_fridge_con())
#' show(test_plot)
get_years_to_go <- function(cur_m_power, new_m_power, new_m_price, el_cost){
  if (cur_m_power - new_m_power < 0){
    years <- Inf
  }
  else{
    new_m_price / ((cur_m_power - new_m_power) * el_cost)
    
  }
}

#' @describeIn get_years_to_go Bilans po pięciu latach
#' @export

get_true_cost <- function(cur_m_power, new_m_power, new_m_price, el_cost){
  new_m_price - (cur_m_power - new_m_power) * 5 * el_cost
}

#' Get info about attributes
#' 
#' @param dataset A single `string`, equal to one of the names of datasets. 
#' Only 'fridges' and 'tvs' would be accepted OR
#' A `data.frame`, following format desribed in Details.
#' 
#' @details It is assumed, that each dataset is 
#' a `data.frame`, where ALL columns following column `Zdj` describe attributes.
#' Naturally, it has to contain EXACTLY ONE column `Zdj`.
#' 
#' @return A named `list`. Each element describes 1 attribute as a `list` with 3 elements:
#'  * `name` - a single string with name of attribute
#'  * `type` - possible values: "numeric" or "factor"
#'  * `range` - for "numeric": c(min, max); for "factor" - levels vector
#'  
#' @export
#' 
get_attr_info <- function(dataset){
  e <- rlang::current_env()
  if(!(is.data.frame(dataset) || is.character(dataset) && length(dataset) == 1))
    rlang::abort('`dataset` must be a single `string` or a `data.frame`.')
  if(is.character(dataset)){
    all_datasets <- utils::data(package = 'asystentWymiany')[['results']][,'Item']
    if(!dataset %in% all_datasets)rlang::abort(paste0('Dataset ',dataset, ' not found.'))
    utils::data(list = dataset, package = 'asystentWymiany', envir = e)
    assign('dataset', base::get(dataset, envir = e), envir = e)
  } else if(is.data.frame(dataset) && sum(colnames(dataset) == 'Zdj') != 1)
    rlang::abort(paste0(dataset, ' must have exactly one `Zdj` column, not ', sum(colnames(dataset))))
  cnames <- colnames(dataset)
  chosen_cnames <- cnames[(which(cnames == 'Zdj') + 1):length(cnames)]
  out <- lapply(chosen_cnames, function(cname){
    if(is.numeric(dataset[[cname]])){
      list(
        name = cname,
        type = 'numeric',
        range = c(min = min(dataset[[cname]]), max = max(dataset[[cname]]))
      )
    } else if(is.character(dataset[[cname]]) || is.factor(dataset[[cname]])){
      v <- as.factor(dataset[[cname]])
      list(
        name = cname,
        type = 'factor',
        range = attr(v, 'levels')
      )
    } else NULL
  })
  names(out) <- chosen_cnames
  if(any(sapply(out, is.null))){
    warning('Some columns are neither numeric or factor/character. Ignoring')
    out <- out[!sapply(out, is.null)]
  }
  out
}

#' Create slider input
#' 
#' @param name `string` - nazwa tworzonego slidera
#' @param range `numeric` dlugosci 2 - granica dolna i gorna tworzonego slitera
#' 
#' @return sliderInput from shiny package based on given label (name) and range
#' 
#' @export
#'
#' @import shiny 
#' @import stringi
#' 
slider_el <- function(name, range) {
  shiny::sliderInput(inputId = paste0("filter__", name, "__slider"),
                     label = paste0(stringi::stri_replace_all_fixed(name, "_", " "),":"),
                     min = range[1], max = range[2],
                     value = range)
}

#' Create drop-down list
#' 
#' @param name `string` - nazwa tworzonego slidera
#' @param range wektor `factor` zawierajacy mozliwe do wyboru opcje
#' 
#' @return selectInput from shiny package based on given label (parametr name) and choices (parametr range)
#'
#' @export
#'
#' @import shiny 
#' @import stringi
#' 
list_el <- function(name, range) {
  shiny::selectInput(inputId = paste0("filter__", name, "__list"),
                     label = paste0(stringi::stri_replace_all_fixed(name, "_", " "),":"),
                     choices = range, multiple = TRUE, selected = range)
}

#' Create filters as sliders and drop-down lists
#' 
#' @export
#' 
#' @param attr_list `list`a generowana przez \code{\link[asystentWymiany]{get_attr_info}}
#' 
#' @return named list of filters
#'
create_filters_elements <- function(attr_list) {
  lapply(attr_list, function(list){
    if(identical(list$type, "numeric")){
      slider_el(list$name, list$range)
    }else{
      list_el(list$name, list$range)
    }
  })
}


#' Filter given dataset using a list of filters ('numeric' or 'factor' type)
#' 
#' @param filters generowane w serverze, gdy user kliknie "Zastosuj" w zakładce filtry
#' @param dataset zbior danych o modelach ze sklepow partnerow
#' 
#' @return filtered dataset
#' 
#' @details it is assumed that filters variable has the same structure as in output of get_attr_info function
#' A named `list`. Each element describes 1 filter as a `list` with 3 elements:
#'  * `name` - a single string with name of attribute to filter by
#'  * `type` - possible values: "numeric" or "factor"
#'  * `range` - for "numeric": c(min, max); for "factor" - levels vector to filter by
#' 
#' @export
#' 
#' @import dplyr
#' 
#' @examples
#' temp <- get_attr_info()
#' filters <- list(Szerokosc_cm=temp[[3]], Sterowanie_smartfonem=temp[[8]])
#' filters[[1]][[3]] <- c(min = 60, max = 80)
#' filters[[2]][[3]] <- c('Tak')
#' test <- filter_by_attr(filters = filters, dataset = fridges)
#' 
#' 
filter_by_attr <- function(filters, dataset) {
  if(all(is.na(filters))) {
    return(dataset)
  }
  for(filter in filters) {
    name <- filter$name
    type <- filter$type
    range <- filter$range
    if(identical(type,'numeric')) {
      dataset <- dplyr::filter(dataset, .data[[name]] >= floor(range['min']), .data[[name]] <= ceiling(range['max']))
    }
    if(identical(type,'factor')) {
      dataset <- dplyr::filter(dataset, .data[[name]] %in% range)
    }
  }
  dataset
}


#' Calculate power consumption per month
#' 
#' @export
#' 
#' @import rlang
#'
#' @return monthly energy consumption data.frame with: 
#' Month - month number as numeric
#' FGE - monthly average power level in W as numeric
#' kWh - monthly energy usage in kWh as numeric 
#' 
get_fridge_con <- function(){
  utils::data("electricity", package = "asystentWymiany", envir = rlang::current_env())
  fridge_con <- Electricity[, c("FGE", "Month")]
  monthly_con <- stats::aggregate(FGE ~ Month, fridge_con, mean)
  monthly_con$kWh <- monthly_con$FGE * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) * 24 / 1000
  monthly_con$Month <- as.numeric(monthly_con$Month)
  round(monthly_con)
}

#' Calculate power consumption and usage time per month for current tv
#' @import stats
#'
#' @export
#' @return monthly energy consumption data.frame with: 
#' Month - month number as numeric
#' TVE - monthly average power level in W as numeric
#' kWh - monthly energy usage in kWh as numeric 
#' Off - average number of minutes with turned off tv /each month/ as 
#' On - average monthly number of minutes with turned on tv /each month
get_tv_con <- function(){
  utils::data("electricity", package = "asystentWymiany", envir = rlang::current_env())
  tv_time <- Electricity[, c("UNIX_TS", "TVE")]
  tv_time$Month <- format(as.POSIXlt(tv_time$UNIX_TS, origin = "1970-01-01"), "%m")
  months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  monthly_con <- stats::aggregate(TVE ~ Month, tv_time, mean) # Mean monthly power consuption 
  monthly_con$kWh <- monthly_con$TVE * months * 24 / 1000
  
  tv_time$Usage <- tv_time$TVE > 60 # 60 to zużycie graniczne powyżej którego interpretujemy TV jako włączony
  summed_time <- table(tv_time$Usage, tv_time$Month)[1,]  + table(tv_time$Usage, tv_time$Month)[2,]
  monthly_con$Off <- table(tv_time$Usage, tv_time$Month)[1,] / summed_time * months * 24 * 60# average number of minutes in each month with turned off tv
  monthly_con$On <- table(tv_time$Usage, tv_time$Month)[2,] / summed_time* months * 24 * 60 #turned on
  monthly_con
}

#' Calculate power consumption per month for new tv
#' @param stand_con stanby power consumption
#' @param on_con running power consumption
#' @param monthly_con dataset produced by get_fridge_con
#' 
#' @export
#' @return monthly energy consumption vector in kWh
get_new_tv_con <- function(stand_con, on_con, monthly_con){
  monthly_con$On * on_con / (60 * 1000) + monthly_con$Off*stand_con / (60 * 1000) # to kWh
}


#' Get offers for a given model and type of device
#' 
#' @param id id of the device for which we want to get offers
#' @param type_of_device `string` that determines the type of device.
#' For now, only 'fridges' or 'tvs' would be accepted 
#' 
#' @return `list`` with four elements, every of which contains: Cena, Sklep, URL, NR
#'
#' @export
#' @import rlang
#' @import magrittr
#' @import purrr
#' @import shinydashboard
#' 
get_offers <- function(id, type_of_device){
  if(!type_of_device %in% c("fridges","tvs"))rlang::abort(paste0('Type of device not found.'))
  all_offers <- base::get(utils::data(list = paste0(type_of_device, '_offers'), package = 'asystentWymiany', envir = rlang::current_env()))
  all_offers <- all_offers[all_offers$ID == id, c("Cena","Sklep","URL")]
  all_offers <- cbind(all_offers, NR = c(1:dim(all_offers)[1])) %>% purrr::transpose()
  all_offers
}


#' Ustala kolejnosc kafelkow na interfejsie 1
#' 
#' Na podstawie aktulanego zurzycia energii znajduje najkorzystniejsze wymiany ze wzgledu na wybrane przez uzytkownika kryterium
#' 
#' @param cur_m_powers wektor aktualnego rocznego zurzycia pradu przez kolejne urzadzenia
#' @inheritParams get_best_models
#' 
#' @return wektor `c("fridges", "tvs", "kettles", "washing-machines", "air-conditionings", "microwaves")` posortowanych pod wzgledem danego kryterium.
#' @export
get_kolejnosc_kafelkow_interface1 <- function(cur_m_power_fridge, tv_con, el_cost, criterion){
  urzadzenia <- c("fridges", "tvs")
  n <- length(urzadzenia)
  
  
  wartosci_kryteriow <- numeric(n)
  for(i in 1:n){
    wartosci_kryteriow[i] <- get_best_models(urzadzenia[i],
                                             cur_m_power_fridge,
                                             tv_con,
                                             el_cost,
                                             top_n = 1,
                                             criterion = criterion)$criterion
  }
  
  out <- urzadzenia[kolejnosc_kryteriow(wartosci_kryteriow, criterion)] # ustawienie urzadzen w odpowiedniej kolejnosci
  
  out <- c(out, "kettles", "washing-machines", "microwaves", "air-conditionings") # sztucznie dodano wiecej urzadzen
  
  attr(out, "do_wyswietlania") <- c(tekst_do_wyswietlania(wartosci_kryteriow[kolejnosc_kryteriow(wartosci_kryteriow, criterion)], # ustawienie tekstow w odpowiedniej kolejnosci
                                                          criterion, el_cost),
                                    rep("Niedostepne", 4))
  
  out
}



#' Ustala kolejnosc kryterium od najleprzego do najgorszego
#' 
#' Wedlog jednego z kryteriow leprze sa te modele, ktore maja wartosc najwieksza, wedlog pozostalych dwoch - najmniejsza
#' 
#' @param wartosci_kryteriow Wartosci zwracane przez `get_best_models` w kolumnie `criterion`
#' @inheritParams get_best_models
#' 
#' @return permuteację zbioru 1:n ustalajacego kolenosc `wartosci_kryteriow`
#' @export
kolejnosc_kryteriow <- function(wartosci_kryteriow, criterion){
  if(criterion == "power_efficiency")
    return(order(-wartosci_kryteriow))
  return(order(wartosci_kryteriow))
}

#' Zamienia date wyrazona w latach (np. 2.5 lat) na liczbe lat i miesiecy (np. 2 lata i 6 miesiecy) 
#' 
#' @param num_years liczba lat
#' 
#' @return tekst z odpowiednia liczba lat i miesiecy
#' @import lubridate
#' @export
wyswietl_date <-  function(num_years){
  tekst_zwrot <- c()
  for (n in num_years){
    y <- lubridate::year(lubridate::date_decimal(n))
    m <- lubridate::month(lubridate::date_decimal(n))
    tekst <- c("lata","miesiace")
    ifelse(y == 1, tekst[1] <- "rok", ifelse(y== 0 | y >= 5, tekst[1] <- "lat", tekst[1] <- "lata"))
    ifelse(m == 1, tekst[2] <- "miesiąc", ifelse(m >= 5, tekst[2] <- "miesięcy", tekst[2] <- "miesiące"))
    ifelse(y==0,tekst_zwrot <- append(tekst_zwrot,paste(m,tekst[2])),tekst_zwrot <- append(tekst_zwrot,paste(y,tekst[1],m,tekst[2])))
    
  }
    return(tekst_zwrot)
}

#' Ustala jakie napisy powinny sie wyswietlac na kafelkach interfejsu 1
#' 
#' @inheritParams kolejnosc_kryteriow
#' @inheritParams get_best_models
#' 
#' @return wektor napisow majacych byc wyswietlane na interfejsie 1
#' @export
tekst_do_wyswietlania <- function(wartosci_kryteriow, criterion, el_cost){
  return(switch(criterion,
                'years_to_go' = paste0("Zwrot już za ", wyswietl_date(round(wartosci_kryteriow, 2))),
                'prize' = paste0("Już od ", wartosci_kryteriow, " zł"),
                'power_efficiency' = paste0("Zaoszczędź ", round(wartosci_kryteriow * el_cost, 2), " zł miesięcznie"), # zaoszczedzonych
                'true_cost' = ifelse(wartosci_kryteriow < 0,
                                     paste0("Zaoszczędź ", -round(wartosci_kryteriow, 0), " zł po 5 latach"),
                                     paste0("Zapłać tylko ", round(wartosci_kryteriow, 0), " zł po 5 latach"))))
}


