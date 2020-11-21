library(ggplot2)
library(lubridate)
library(tidyr)

yearly_forecast_plot <- function(cur_m_power, new_m_power, new_m_price, el_cost) {
  cur_date <- today()
  years_to_go <- new_m_price / ((cur_m_power - new_m_power) * el_cost)
  end_date <- cur_date + days(round(365 * years_to_go))
  fut_date <- cur_date + days(round(1.2 * 365 * years_to_go))
  time <- c(cur_date, fut_date)
  
  meet_cost = cur_m_power * years_to_go * el_cost
  cur_m_cost = c(0, 1.2 * cur_m_power * years_to_go * el_cost)
  new_m_cost = new_m_price + c(0, 1.2 * new_m_power * years_to_go * el_cost)
  
  plot_data = data.frame(time, cur_m_cost, new_m_cost) %>% gather(variable, value, -time)
  
  x_breaks <- c(seq(from = cur_date, to = end_date, by = 'year'), end_date)
  x_breaks <- c(x_breaks, seq(from = x_breaks[(match(end_date, x_breaks)-1)]+years(1), to = fut_date, by = 'year'))
  # x_breaks <- c(x_breaks, seq(from = x_breaks[(match(end_date, x_breaks)-1)]+years(1), to = fut_date, by = 'year'), fut_date)
  
  if(end_date - x_breaks[match(end_date, x_breaks) - 1] < 90) {
    x_breaks <- x_breaks[-(match(end_date, x_breaks) - 1)]
  }
  
  # if(fut_date - x_breaks[match(fut_date, x_breaks) - 1] < 90) {
  #   x_breaks <- x_breaks[-(match(fut_date, x_breaks) - 1)]
  # }
  
  plot <- ggplot(data = plot_data, aes(x = time, y = value, col = variable)) +
    geom_line() +
    geom_point() +
    geom_point(x = end_date, y = meet_cost, col = 'black') +
    geom_segment(x = end_date, y = -meet_cost, xend = end_date, yend = meet_cost, col = 'black', linetype = 'dashed', size = 0.5) +
    scale_x_date(breaks = x_breaks) +
    scale_color_manual(name = 'Model:', labels = c('obecny', 'proponowany'), values = c('#ec524b', '#16a596')) +
    xlab('Data') +
    ylab('Skumulowany koszt (PLN)') +
    annotate(geom = 'text', x = cur_date + (fut_date - cur_date) / 2, y = 0, label = paste('Lat do zwrotu inwestycji:', round(years_to_go, 2))) +
    annotate(geom = 'text', x = cur_date + (fut_date - cur_date) / 2, y = 0.05 * max(plot_data$value), label = paste('Koszt zakupu:', round(new_m_price, 2), 'PLN')) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = 'top',
          panel.grid.minor.x = element_blank()
    )
  return(plot)
}

cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)

test_plot <- yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost)

show(test_plot)

