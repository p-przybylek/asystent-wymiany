cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)
cur_month_power <- get_fridge_con()

test_that('get_years_to_go works',{
          expect_equal(get_years_to_go(cur_m_power = 100, new_m_power = 50, new_m_price = 0, el_cost = 1),0)
          expect_equal(get_years_to_go(cur_m_power = 100, new_m_power = 50, new_m_price = 100, el_cost = 1),2)
  })

test_that("Plot works and proper data",{
  p1 <- yearly_forecast_plot(cur_m_power, new_m_power, new_m_price, el_cost, cur_month_power)
  expect_s3_class(p1, 'ggplot')
  expect_equal(p1$data$time[1], lubridate::today()) # pierwszy wyświetlany dzień to dzisiaj
  expect_equal(p1$data$value[1], 0) # dziś koszt używania aktualnego użądzenia wynosi 0
  
  n <- length(p1$data$time)
  expect_equal(n %% 2, 0) # każdej dacie odpowiada wartość aktualnego i proponowanego modelu
  expect_equal(p1$data$time[seq(1, n, by=2)], p1$data$time[seq(2, n, by=2)]) # daty są takie same
  expect_equal(unique(p1$data$variable), c("cur_m_cost", "new_m_cost"))
  })
         




