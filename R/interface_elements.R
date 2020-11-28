#' Create tile for the first interface
#'
#' @return box with text
#' 
#' @import shinydashboard

box_interfejs1 <- function(id, text){
  return(shinydashboard::box(width = 4, class = "box", id = id, p(text)))
}
