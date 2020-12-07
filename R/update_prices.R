#' Updates prices to the minimum of the proposed offers for a given type of device
#'  
#' @param type_of_device `string` that determines the type of device.
#' For now, only 'fridges' or 'tvs' would be accepted 
#' 
#' @return 
#' 
#' @export
#' @import rlang
#' @import dplyr
#' 
update_prices <- function(type_of_device){
  if(!type_of_device %in% c("fridges","tvs"))rlang::abort(paste0('Type of device not found.'))
  devices_offers <- base::get(utils::data(list = paste0(type_of_device, '_offers'), package = 'asystentWymiany', envir = rlang::current_env()))
  devices <- base::get(utils::data(list = type_of_device, package = 'asystentWymiany', envir = rlang::current_env()))
  
  minCena <- devices_offers %>% group_by(ID) %>% summarise(minCena = min(Cena))
  devices <- left_join(devices,minCena,by="ID")
  devices[,"Cena"] <- devices[,"minCena"]
  devices <- subset(devices,select= -minCena)
  assign(paste0(type_of_device),devices)
  save(list = type_of_device, file=paste0("data/", type_of_device, ".rda"))
}