#----- get_best_fridges() -----
cur_m_power <- 400 # kWh/rok
new_m_power <- 170 # kWh/rok
new_m_price <- 1200 # PLN
el_cost <- 0.617 # PLN/kWh, średni koszt energii elektrycznej w Polsce (maj 2020)
test_that('No errors on get_best_fridges',{
  expect_s3_class(get_best_fridges(cur_m_power, el_cost, criterion = 'years_to_go'),'data.frame')
  expect_setequal(colnames(get_best_fridges(cur_m_power, el_cost, criterion = 'years_to_go')), c('ID', 'Nazwa', 'Cena', 'Roczne_zuzycie_pradu_kWh', 'criterion'))
})
test_that('top_n works',{
  expect_lte(nrow(get_best_fridges(cur_m_power, el_cost, top_n = 10, criterion = 'years_to_go')),
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


tv_con <- get_tv_con()
test_that("get_best_models zwroci NA na poczatku dzialania aplikacji", {
  expect_null(get_best_models(NA, 60, 0.617, tv_con))
})

test_that("get_best_models poprawnie obudowuje get_best_((fridges)|(tvs))", {
  expect_true(dplyr::all_equal(get_best_models("fridges", 1200, NULL, 0.617),
                               get_best_fridges(1200, 0.617, criterion = 'years_to_go')))
  expect_true(dplyr::all_equal(get_best_models("tvs", NULL, tv_con, 0.617),
                               get_best_tvs(tv_con, 0.617, criterion = 'years_to_go')))
})

#----- get_best_models z parametrem criterion -----
test_that('Error dla niepoprawnego criterion',{
  expect_error(get_best_models('fridges', 1200, 0.617, criterion = TRUE))
  expect_error(get_best_models('fridges', 1200, 0.617, criterion = c('prize', 'power_efficiency')))
  expect_error(get_best_models('fridges', 1200, 0.617, criterion = 'TRUE'))
})

test_that('get_best_models z poprawnym parametrem criterion nie zwraca błędu',{
  expect_silent(fridges_by_power <- get_best_models('fridges', 1200, NULL, 0.617, criterion = 'power_efficiency'))
  expect_silent(get_best_models('fridges', 1200, NULL, 0.617, criterion = 'prize'))
  expect_silent(get_best_models('fridges', 1200, NULL, 0.617, criterion = 'years_to_go'))
  expect_silent(get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'power_efficiency'))
  expect_silent(get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'prize'))
  expect_silent(get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'years_to_go'))
})

test_that('get_best_models działa poprawnie z parametrem criterion dla lodowek',{
  fridges_by_power <- get_best_models('fridges', 1200, NULL, 0.617, criterion = 'power_efficiency')
  fridges_by_prize <- get_best_models('fridges', 1200, NULL, 0.617, criterion = 'prize')
  fridges_by_years <- get_best_models('fridges', 1200, NULL, 0.617, criterion = 'years_to_go')
  expected_fridges_colnames <- c('ID', "Nazwa", "Cena", "Roczne_zuzycie_pradu_kWh", 'criterion')

  expect_s3_class(fridges_by_power, 'data.frame')
  expect_setequal(colnames(fridges_by_power), expected_fridges_colnames)
  expect_s3_class(fridges_by_prize, 'data.frame')
  expect_setequal(colnames(fridges_by_prize), expected_fridges_colnames)
  expect_s3_class(fridges_by_years, 'data.frame')
  expect_setequal(colnames(fridges_by_years), expected_fridges_colnames)
})

test_that('get_best_models działa poprawnie z parametrem criterion dla telewizorow',{
  tvs_by_power <- get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'power_efficiency')
  tvs_by_prize <- get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'prize')
  tvs_by_years <- get_best_models('tvs', NULL, tv_con, 0.617, criterion = 'years_to_go')
  expected_tvs_colnames <- c('ID', "Nazwa", "Cena", "Pobor_mocy_tryb_czuwania_W", "Pobor_mocy_tryb_wlaczenia_W", "years_to_go", 'criterion')

  expect_s3_class(tvs_by_power, 'data.frame')
  expect_setequal(colnames(tvs_by_power), expected_tvs_colnames)
  expect_s3_class(tvs_by_prize, 'data.frame')
  expect_setequal(colnames(tvs_by_prize), expected_tvs_colnames)
  expect_s3_class(tvs_by_years, 'data.frame')
  expect_setequal(colnames(tvs_by_years), expected_tvs_colnames)
})

#----- kafelki interfejs 1 -----
test_that("kolejnosc_kryteriow daje poprawna kolejnosc dla wszystkich mzliwych kryteriow", {
  expect_equal(c(2, 1, 3), kolejnosc_kryteriow(c(600, 300, 900), "years_to_go"))
  expect_equal(c(2, 1, 3), kolejnosc_kryteriow(c(600, 300, 900), "prize"))
  expect_equal(c(3, 1, 2), kolejnosc_kryteriow(c(600, 300, 900), "power_efficiency"))
})

test_that("tekst_do_wyswietlania daje poprawny tekst", {
  expect_equal(c("600 lat", "300 lat", "900 lat"), tekst_do_wyswietlania(c(600, 300, 900), "years_to_go"))
  expect_equal(c("600 zł", "300 zł", "900 zł"), tekst_do_wyswietlania(c(600, 300, 900), "prize"))
  expect_equal(c("600 zł miesięcznie", "300 zł miesięcznie", "900 zł miesięcznie"), tekst_do_wyswietlania(c(600/0.617, 300/0.617, 900/0.617), "power_efficiency", 0.617))
})


