cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)

test_that('get_years_to_go works',
          expect_equal(get_time_to_go(cur_m_power = 100, new_m_power = 50, new_m_price = 0, el_cost = 1),0),
          expect_equal(get_time_to_go(cur_m_power = 100, new_m_power = 50, new_m_price = 100, el_cost = 1),2))

test_that("Plot works",
         expect_s3_class(yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost),'ggplot'))