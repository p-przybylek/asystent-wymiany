#' Create tile for the first interface
#' 
#' Używany w app_ui.R. Generuje boxy widoczne na pierwszym interfejsie aplikacji.
#'
#' @param id `string` - id boxa w CSS
#' @param text `string` - Teks w środku boxa
#'
#' @return box z tekstem
#' 
#' @export
#' 
#' @import shinydashboard

box_interfejs1 <- function(id, text){
  return(shinydashboard::box(width = 4, class = "box", id = id, p(text)))
}
