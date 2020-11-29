#' Get best fridges
#' 
#' @inheritParams yearly_forecast_plot
#' @param top_n liczba najleprzych modeli do pokazania
#' @param filters filtry do zaaplikowania (być może NA)
#' 
#' @return A data.frame with 4 columns: ID, Nazwa, Cena, Roczne_zuzycie_pradu_kWh
#' with info about most cost-efficient top_n fridges 
#' @export
#' 
get_best_fridges <- function(cur_m_power, el_cost, top_n=5, filters = NA){
  data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  fridges <- filter_by_attr(filters = filters, dataset = fridges)
  fridges$years_to_go <- sapply(1:nrow(fridges), function(i){
    get_years_to_go(cur_m_power, 
                    new_m_power = fridges[i,'Roczne_zuzycie_pradu_kWh'],
                    new_m_price = fridges[i,'Cena'],
                    el_cost)
  })
  fridges[order(fridges$years_to_go)[1:top_n],c('ID', "Nazwa", "Cena", "Roczne_zuzycie_pradu_kWh")]
}

#' Get info about attributes
#' 
#' @param dataset A single `string`, equal to one of the names of datasets. 
#' For now, only 'fridges' would be accepted OR
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
get_attr_info <- function(dataset = 'fridges'){
  e <- rlang::current_env()
  if(!(is.data.frame(dataset) || is.character(dataset) && length(dataset) == 1))
    rlang::abort('`dataset` must be a single `string` or a `data.frame`.')
  if(is.character(dataset)){
    all_datasets <- data(package = 'asystentWymiany')[['results']][,'Item']
    if(!dataset %in% all_datasets)rlang::abort(paste0('Dataset ',dataset, ' not found.'))
    data(list = dataset, package = 'asystentWymiany', envir = e)
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
                     choices = range)
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
  if(is.na(filters)) {
    return(dataset)
  }
  for(filter in filters) {
    name <- filter$name
    type <- filter$type
    range <- filter$range
    if(type == 'numeric') {
      dataset <- dplyr::filter(dataset, .data[[name]] > range['min'], .data[[name]] < range['max'])
    }
    if(type == 'factor') {
      dataset <- dplyr::filter(dataset, .data[[name]] %in% range)
    }
  }
  dataset
}


#' Calculate power consumption per month
#' 
#' @export
#'
#' @return monthly energy consumption data.frame with: 
#' Month - month number as numeric
#' FGE - monthly average power level in W as numeric
#' kWh - monthly energy usage in kWh as numeric 
#' 
get_fridge_con <- function(){
  data("electricity", package = "asystentWymiany", envir = rlang::current_env())
  fridge_con <- Electricity[, c("FGE", "Month")]
  monthly_con <- aggregate(FGE ~ Month, fridge_con, mean)
  monthly_con$kWh <- monthly_con$FGE * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) * 24 / 1000
  monthly_con$Month <- as.numeric(monthly_con$Month)
  round(monthly_con)
}
