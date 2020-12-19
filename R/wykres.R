#' Yearly forecast plot
#' 
#' Tworzy wykres predykcji
#' 
#' @param cur_m_power moc aktualne posiadanego przez urzytkownika uzadzenia
#' @param new_m_power moc proponowanego uzadzenia
#' @param new_m_price cena proponowanego uzadzenia
#' @param el_cost koszt pradu, ktore zuzywaja uzadzenia
#' @param cur_month_power tabela zurzycia pradu przez aktualne uzadzenie w zaleznosci od miesiaca
#' 
#' @return wykres w pplot2
#' 
#' @export
#'
#' @import ggplot2
#' @import lubridate
#' 
#' @examples 
#' yearly_forecast_plot(400, 170, 1200, 0.617, get_fridge_con())

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
    
  plot <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = time, y = value, col = variable)) +
    ggplot2::geom_line(size = 2) +
    ggplot2::geom_segment(x = end_date, y = -meet_cost, xend = end_date, yend = meet_cost, col = 'black', linetype = 'dashed', size = 0.5) +
    ggplot2::annotate(geom = 'label', x = end_date, y = meet_cost / 2 , label = paste0('Data zwrotu inwestycji: \n', end_date), size = 5) +
    ggplot2::scale_color_manual(name = 'Model:', labels = c('obecny', 'proponowany'), values = c('#ec524b', '#16a596'), guide = guide_legend(reverse = TRUE)) +
    ggplot2::xlab('Data') +
    ggplot2::ylab('Skumulowany koszt (zł)') +
    ggplot2::coord_cartesian(ylim = c(0, max(plot_data$value)*1.2)) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          legend.position = 'top',
          panel.grid.minor.x = ggplot2::element_blank()
    )
  plot <- plot + ggplot2::annotate(geom = 'label', x = cur_date + (fut_date - cur_date) / 2, y = max(plot_data$value)*1.15, label = paste('Cena zakupu: ', format(round(new_m_price, 2), nsmall=2), 'zł \n Lat do zwrotu inwestycji: ', format(round(years_to_go, 2), nsmall=2)), fill = '#16a596', colour = 'white', fontface = 'bold', size = 6)
  return(plot)
}

#' yearly_data_to_plot
#' 
#' Tworzy dataset potrzebny do wykresu TV
#' 
#' @param cur_m_power roczne zużycie prądu aktualnie posiadanego przez urzytkownika urzadzenia
#' @param new_m_power roczne zużycie prądu proponowanego przez urzytkownika urzadzenia
#' @param new_m_price cena proponowanego urzadzenia
#' @param el_cost koszt pradu, ktore zuzywaja urzadzenia
#' @param cur_month_power tabela zuzycia pradu przez aktualne urzadzenie w zaleznosci od miesiaca
#' @param new_month_powera wektor zuzycia pradu przez aktualne urzadzenie w zaleznosci od miesiaca
#' 
#' @return dataframe
#' 
#' @export
#'
#' @import lubridate
#' 
#' @examples 
#' stand_con <- 0.5
#' on_con <- 200
#' cur_month_power <- get_tv_con() - from server_utils
#' new_month_power <- get_new_tv_con(stand_con, on_con, cur_month_power) - from server_utils
#' new_m_power <- sum(new_month_power)
#' cur_m_power <- sum(cur_month_power$kWh)
#' el_cost <- 0.617
#' new_m_price <- 1000
#' yearly_data_to_plot(cur_m_power, new_m_power, new_m_price, el_cost, cur_month_power, new_month_power)
yearly_data_to_plot <- function(cur_m_power, new_m_power, new_m_price, el_cost, cur_month_power, new_month_power) {
  
  cur_date <- lubridate::today()
  years_to_go <- get_years_to_go(cur_m_power, new_m_power, new_m_price, el_cost)
  end_date <- cur_date + lubridate::days(round(365 * years_to_go))
  if (years_to_go < 1) {
    fut_date <- cur_date + lubridate::days(365)
  } else{
    fut_date <- cur_date + lubridate::days(round(1.2 * 365 * years_to_go))
  }
  
  
  inter_date <- seq(lubridate::ceiling_date(cur_date, unit = "month"),
                    lubridate::floor_date(fut_date, unit = "month"), by = "months")
  
  to_next_month_cost <- as.numeric(lubridate::ceiling_date(cur_date, unit = "month") - cur_date) *
    cur_month_power$kWh[lubridate::month(cur_date)] / 30 * el_cost
  to_last_month_cost <- abs(as.numeric(lubridate::floor_date(fut_date, unit = "month") - fut_date)) *
    cur_month_power$kWh[lubridate::month(fut_date)] / 30 * el_cost
  
  to_next_month_cost_new <- as.numeric(lubridate::ceiling_date(cur_date, unit = "month") - cur_date) *
    new_month_power[lubridate::month(cur_date)] / 30 * el_cost
  to_last_month_cost_new <- abs(as.numeric(lubridate::floor_date(fut_date, unit = "month") - fut_date)) *
    new_month_power[lubridate::month(fut_date)] / 30 * el_cost
  month_cost <- numeric(length(inter_date) - 1 )
  month_cost_new <- numeric(length(inter_date) -1)
  for(i in 2:length(inter_date)){
    month_cost[i-1] <- cur_month_power$kWh[lubridate::month(inter_date[i])] * el_cost
    month_cost_new[i-1] <- new_month_power[lubridate::month(inter_date[i])] * el_cost
  }
  cost_cum <- cumsum(c(0, to_next_month_cost, month_cost, to_last_month_cost))
  cost_cum_new <- cumsum(c(new_m_price, to_next_month_cost_new, month_cost_new, to_last_month_cost_new))
  
  time <- c(cur_date, inter_date, fut_date)
  meet_cost = cur_m_power * years_to_go * el_cost
  cur_m_cost = c(cost_cum)
  new_m_cost = c(cost_cum_new)
  
  plot_data <- tidyr::pivot_longer(data.frame(time, cur_m_cost, new_m_cost),
                                   cols = -time, 
                                   names_to = "variable", 
                                   values_to = "value")
}

#' @import magrittr
#' @import tidyr
monthly_savings_plot <- function(monthly_con, new_m_price, years_to_go, el_cost) {
  months <- c('Styczeń', 'Luty', 'Marzec', 'Kwiecień', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpień', 'Wrzesień', 'Październik', 'Listopad', 'Grudzień')
  plot_data <- data.frame(month = factor(x = months, levels = months), old = monthly_con$old * el_cost, new = monthly_con$new * el_cost)
  plot_data <- plot_data %>% tidyr::pivot_longer(cols = -month, names_to = 'variable', values_to = 'value')
  arrow_data <- data.frame(x = seq(1, 12, 1) - 0.125, y = monthly_con$old * el_cost, yend = monthly_con$new * el_cost)
  label_data <- data.frame(x = seq(1, 12, 1) - 0.125, y = (monthly_con$old * el_cost - monthly_con$new * el_cost) / 2 + monthly_con$new * el_cost, label = format(round(monthly_con$old*el_cost - monthly_con$new*el_cost, 2), nsmall=2))
  plot <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = month, y = value, fill = variable)) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
    ggplot2::xlab('Miesiąc') +
    ggplot2::ylab('Koszt użytkowania (zł)') +
    ggplot2::coord_cartesian(ylim = c(0, max(plot_data$value)*1.2)) +
    ggplot2::scale_fill_manual(name = 'Model:', labels = c('proponowany', 'obecny'), values = c('#16a596', '#ec524b')) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      plot.margin = margin(1, 1, 1, 1, "cm"),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      legend.position = 'top'
    )
  for(i in 1:12) {
    plot <- plot +
      ggplot2::annotate(geom = 'segment', x = arrow_data$x[i], xend = arrow_data$x[i], y = arrow_data$y[i], yend = arrow_data$yend[i], arrow = ggplot2::arrow(ends = 'last')) +
      ggplot2::annotate(geom = 'text', x = label_data$x[i], y = label_data$y[i], label = label_data$label[i], colour = '#16a596', angle = 90, vjust = -0.2, size = 5, fontface = 'bold')
  }
  plot <- plot + ggplot2::annotate(geom = 'label', x = 6.5, y = max(plot_data$value)*1.15, label = paste('Cena zakupu: ', format(round(sum(new_m_price), 2), nsmall=2), 'zł \n Roczna oszczędność: ', format(round(sum(monthly_con$old*el_cost - monthly_con$new*el_cost), 2), nsmall=2), 'zł'), fill = '#16a596', colour = 'white', fontface = 'bold', size = 6)
  return(plot)
}