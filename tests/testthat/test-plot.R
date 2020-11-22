cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)

testthat("Plot works",
         expect_s3_class(yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost),'ggplot'))