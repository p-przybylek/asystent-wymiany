#' @importFrom ddpcr quiet
ddpcr::quiet( # dzieki ddpcr::quiet nie printuje sie output
test_that('sprawdz_poprawnosc_lodowek wykryje bledne dane',{
  utils::data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  poprawne_dane <- fridges
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,2] <- "Bez 'Lodowka' na poczatku"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,3] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,4] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,5] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,6] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,7] <- NA
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,8] <- NA
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,9] <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,10] <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,11] <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,12] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,13] <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,14] <- "FALSE"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,15] <- "FALSE"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,16] <- "TRUE"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,17] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,18] <- NA
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane[1,'Klasa_energetyczna'] <- 'Z' 
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
}))

test_that('fridges są poprawne',{
  utils::data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  expect_true(sprawdz_poprawnosc_lodowek(fridges))
})

ddpcr::quiet(
  test_that('sprawdz_poprawnosc_tv wykryje bledne dane',{
    
    utils::data('tvs', package = 'asystentWymiany', envir = rlang::current_env())
    poprawne_dane <- tvs
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,1] <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[2,2] <- "Bez 'Telewizor' na poczatku"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,3] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,4] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,5] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,4] <- 10
    bledne_dane[1,5] <- 5
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,6] <- rep(0, length(bledne_dane[,6]))
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,7] <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,8] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,9] <- rep(0, length(bledne_dane[,9]))
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,10] <- -1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,11] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,12] <- "idk"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,13] <- -1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,14] <- "chyba tak"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,15] <- -1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[1,16] <- "TRUE"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,17] <- "duzo"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,18] <- rep(1, length(bledne_dane[,18]))
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,19] <- "extra"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane[,20] <- "a"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
  }))

test_that('tvs są poprawne',{
  utils::data('tvs', package = 'asystentWymiany', envir = rlang::current_env())
  expect_true(sprawdz_poprawnosc_tv(tvs))
})