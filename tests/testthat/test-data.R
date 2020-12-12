#' @importFrom ddpcr quiet
ddpcr::quiet( # dzieki ddpcr::quiet nie printuje sie output
test_that('sprawdz_poprawnosc_lodowek wykryje bledne dane',{
  utils::data('fridges', package = 'asystentWymiany', envir = rlang::current_env())
  poprawne_dane <- fridges
  
  bledne_dane <- poprawne_dane
  bledne_dane$ID[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Nazwa[1] <- "Bez 'Lodowka' na poczatku"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Cena[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Roczne_zuzycie_pradu_kWh[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Prognozowane_koszty_roczne_PLN[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Srednia_moc_W[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Zdj <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Firma <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Wysokosc_cm[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Szerokosc_cm[1] <- -1
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Glebokosc_cm <- FALSE
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Pojemnosc_uzytkowa_zamrazarki_l[1] <- -12
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Bezszronowa_No_Frost[1] <- "TRUE"
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Sterowanie_smartfonem[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Zmiana_kierunku_otwierania_drzwi <- NA
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane
  bledne_dane$Poziom_halasu_dB[1] <- 0
  expect_false(sprawdz_poprawnosc_lodowek(bledne_dane))
  
  bledne_dane <- poprawne_dane 
  bledne_dane$Kolor <- TRUE
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
    bledne_dane$ID[1] <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$ID[1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Nazwa[2] <- "Bez 'Telewizor' na poczatku"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Cena[1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Pobor_mocy_tryb_czuwania_W[1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Pobor_mocy_tryb_wlaczenia_W[1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Pobor_mocy_tryb_czuwania_W[1] <- 10
    bledne_dane$Pobor_mocy_tryb_wlaczenia_W[1] <- 5
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Szerokosc_ekranu_px <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Wysokosc_ekranu_px[1] <- 0
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Dla_graczy <- "True"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Czestotliwosc_odswiezania_ekranu_Hz[1] <- "True"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Zdj[1] <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Firma[1] <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Przekatna_ekranu_cal <- NA
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Standard_ekranu <- -1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Smart_TV <- "chyba tak"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Android_TV <- -1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Technologia_HDR_High_Dynamic_Range <- "TRUE"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Zlacza_HDMI <- "duzo"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Zlacza_USB <- "malo"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Kolor_obudowy <- 1
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
    bledne_dane <- poprawne_dane
    bledne_dane$Klasa_energetyczna <- "a"
    expect_false(sprawdz_poprawnosc_tv(bledne_dane))
    
  }))

test_that('tvs są poprawne',{
  utils::data('tvs', package = 'asystentWymiany', envir = rlang::current_env())
  expect_true(sprawdz_poprawnosc_tv(tvs))
})