#' Yearly forecast plot
#'
#' @import ggplot2
#' @import lubridate
yearly_forecast_plot <- function(cur_m_power, new_m_power, new_m_price, el_cost, cur_month_power) {
  cur_date <- lubridate::today()
  years_to_go <- get_years_to_go(cur_m_power, new_m_power, new_m_price, el_cost)
  end_date <- cur_date + lubridate::days(round(365 * years_to_go))
  fut_date <- cur_date + lubridate::days(round(1.2 * 365 * years_to_go))
  
  inter_date <- seq(lubridate::ceiling_date(cur_date, unit = "month"),
                    lubridate::floor_date(fut_date, unit = "month"), by="months")
  to_next_month_cost <- as.numeric(lubridate::ceiling_date(cur_date, unit = "month") - cur_date) *
                        cur_month_power$kWh[lubridate::month(cur_date)] / 30 * el_cost
  to_last_month_cost <- abs(as.numeric(lubridate::floor_date(fut_date, unit = "month") - fut_date)) *
    cur_month_power$kWh[lubridate::month(fut_date)] / 30 * el_cost
  month_cost <- numeric(length(inter_date) - 1 )
  for(i in 2:length(inter_date)){
    month_cost[i-1] <- cur_month_power$kWh[lubridate::month(inter_date[i])] * el_cost
  }
  cost_cum <- cumsum(c(0, to_next_month_cost, month_cost))
  
  time <- c(cur_date, inter_date, fut_date)
  meet_cost = cur_m_power * years_to_go * el_cost
  cur_m_cost = c(cost_cum, 1.2 * cur_m_power * years_to_go * el_cost)
  new_m_cost = new_m_price + seq(0, 1.2 * new_m_power * years_to_go * el_cost, length.out = length(time))
  
  plot_data <- tidyr::pivot_longer(data.frame(time, cur_m_cost, new_m_cost),
                                  cols = -time, 
                                  names_to = "variable", 
                                  values_to = "value")
    
  x_breaks <- c(seq(from = cur_date, to = end_date, by = 'year'), end_date)
  x_breaks <- c(x_breaks, seq(from = x_breaks[(match(end_date, x_breaks)-1)]+lubridate::years(1), to = fut_date, by = 'year'))
  # x_breaks <- c(x_breaks, seq(from = x_breaks[(match(end_date, x_breaks)-1)]+years(1), to = fut_date, by = 'year'), fut_date)
  
  if(end_date - x_breaks[match(end_date, x_breaks) - 1] < 90) {
    x_breaks <- x_breaks[-(match(end_date, x_breaks) - 1)]
  }
  
  if(x_breaks[match(end_date, x_breaks) + 1] - end_date < 90) {
    x_breaks <- x_breaks[-(match(end_date, x_breaks) + 1)]
  }
  
  # if(fut_date - x_breaks[match(fut_date, x_breaks) - 1] < 90) {
  #   x_breaks <- x_breaks[-(match(fut_date, x_breaks) - 1)]
  # }
  
  plot <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = time, y = value, col = variable)) +
    ggplot2::geom_line() +
    #ggplot2::geom_point() +
    #ggplot2::geom_point(x = end_date, y = meet_cost, col = 'black') +
    ggplot2::geom_segment(x = end_date, y = -meet_cost, xend = end_date, yend = meet_cost, col = 'black', linetype = 'dashed', size = 0.5) +
    ggplot2::scale_x_date(breaks = x_breaks) +
    ggplot2::scale_color_manual(name = 'Model:', labels = c('obecny', 'proponowany'), values = c('#ec524b', '#16a596')) +
    ggplot2::xlab('Data') +
    ggplot2::ylab('Skumulowany koszt (PLN)') +
    ggplot2::annotate(geom = 'text', x = cur_date + (fut_date - cur_date) / 2, y = 0, label = paste('Lat do zwrotu inwestycji:', round(years_to_go, 2))) +
    ggplot2::annotate(geom = 'text', x = cur_date + (fut_date - cur_date) / 2, y = 0.05 * max(plot_data$value), label = paste('Koszt zakupu:', round(new_m_price, 2), 'PLN')) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
          legend.position = 'top',
          panel.grid.minor.x = ggplot2::element_blank()
    )
  return(plot)
}

#' Calculate cost-effectiveness
#' 
#' Return, in how many years the investment will pay off
get_years_to_go <- function(cur_m_power, new_m_power, new_m_price, el_cost){
  if (cur_m_power - new_m_power < 0){
    years <- Inf
  }
  else{
    new_m_price / ((cur_m_power - new_m_power) * el_cost)

  }
}
# 
# cur_m_power <- 400 # kWh/rok
# new_m_power <- 170 # kWh/rok
# new_m_price <- 1200 # PLN
# el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)
# 
# test_plot <- yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost)
# 
# show(test_plot)

