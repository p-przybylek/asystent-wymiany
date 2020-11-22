cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)
test_that('No errors on get_best_fridges',{
  expect_s3_class(get_best_fridges(cur_m_power, el_cost),'data.frame')
  expect_setequal(colnames(get_best_fridges(cur_m_power, el_cost)), c('ID', 'Nazwa', 'Cena', 'Roczne_zuzycie_pradu_kWh'))
})
test_that('top_n works',{
  expect_lte(nrow(get_best_fridges(cur_m_power, el_cost, top_n = 10)),
                  10)
})