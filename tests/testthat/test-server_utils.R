#----- get_best_fridges() -----
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

#----- get_attr_info() -----
test_df <- data.frame(V1 = 1:5,
                      V2 = 1:5,
                      V3 = LETTERS[1:5],
                      Zdj = LETTERS[1:5],
                      V4 = 1:5,
                      V5 = 6:10,
                      V6 = c("A","A","B","B","C"))
expected <- list(
  V4 = list(
    name = 'V4',
    type = 'numeric',
    range = c(1,5)
  ),
  V5 = list(
    name = 'V5',
    type = 'numeric',
    range = c(6,10)
  ),
  V6 = list(
    name = 'V6',
    type = 'factor',
    range = LETTERS[1:3]
  )
)
test_that('Works for string',{
  expect_error(get_attr_info('I am not a dataset'))
  expect_silent(get_attr_info('fridges'))
})
test_that('Works for data.frame',{
  expect_equivalent(get_attr_info(test_df),expected)
  expect_error(get_attr_info(data.frame(V1 = 1:5,
                                        V2 = 1:5)))
  expect_error(get_attr_info({
    test_df_2 <- test_df
    names(test_df_2)[1] <- 'Zdj'
    test_df_2
  }))
})

#----- get_fridge_con() -----
test_that("No error caused by get_fridge_con", {
  expect_s3_class(get_fridge_con(), "data.frame")
})
#----- get_tv_con() -----
test_that("No error caused by get_fridge_con", {
  expect_s3_class(get_tv_con(), "data.frame")
  expect_setequal(dim(get_tv_con()), c(12, 5))
})

#----- get_new_tv_con() -----
test_that("No error caused by get_fridge_con", {
  expect_setequal(length(get_new_tv_con(0.5, 100, get_tv_con())), 12)
})

