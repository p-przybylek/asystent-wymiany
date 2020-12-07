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