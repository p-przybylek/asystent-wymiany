sprawdz_poprawnosc_lodowek <- function(fridges){
  poprawnosc_lodowek <- TRUE
  
  if(any(is.na(fridges))){
    print("W zbiorze są NA")
    poprawnosc_lodowek <- FALSE
  }
  
  n <- dim(fridges)[1]
  if(!all(fridges$ID == 1:n)){
    print("ID powinno zaczynać się od 1 i zwiększać się o 1")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(stringi::stri_sub(fridges$Nazwa, 1, 8) == rep("Lodówka ", n))){
    print("nazwy modeli powinny zaczynać się od 'Lodówka '")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Cena > 0)){
    print("wszystkie modele powinny coś kosztować")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Roczne_zuzycie_pradu_kWh > 0)){
    print("wszystkie modele powinny zużywać prąd")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Prognozowane_koszty_roczne_PLN > 0)){
    print("wszystkie modele powinny zużywać prąd")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Srednia_moc_W > 0)){
    print("wszystkie modele powinny zużywać prąd")
    poprawnosc_lodowek <- FALSE
  }
  if(class(fridges$Zdj) != "character"){
    print("linki do zdjęć powinny być napisami")
    poprawnosc_lodowek <- FALSE
  }
  if(class(fridges$Firma) != "character"){
    print("firmy powinny być napisami")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Wysokosc_cm > 0)){
    print("Lodówna powinna mieć dodatnią wysokosc")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Glebokosc_cm > 0)){
    print("Lodówna powinna mieć dodatnią głębokość")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Szerokosc_cm > 0)){
    print("Lodówna powinna mieć dodatnią szerokość")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Pojemnosc_uzytkowa_chlodziarki_l > 0)){
    print("Lodówna powinna mieć dodatnią pojemność")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Pojemnosc_uzytkowa_zamrazarki_l >= 0)){
    print("Zamrażarka powinna mieć dodatnią pojemność")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(match(unique(fridges$Bezszronowa_No_Frost), c("Pełny No Frost", "Brak", "Zamrażarka"), nomatch = 0) > 0)){
    print("Jedyne dozwolone wartości zmiennej 'Bezszronowa_No_Frost' to: 'Pełny No Frost', 'Brak', 'Zamrażarka'")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(match(unique(fridges$Sterowanie_smartfonem), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Jedyne dozwolone wartości zmiennej 'Sterowanie_smartfonem' to: 'Tak', 'Nie'")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(match(unique(fridges$Zmiana_kierunku_otwierania_drzwi), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Jedyne dozwolone wartości zmiennej 'Zmiana_kierunku_otwierania_drzwi' to: 'Tak', 'Nie'")
    poprawnosc_lodowek <- FALSE
  }
  if(!all(fridges$Poziom_halasu_dB > 0)){
    print("'Poziom_halasu_dB' powinien być dodatni")
    poprawnosc_lodowek <- FALSE
  }
  if(class(fridges$Kolor) != "character"){
    print("Kolor powinien być napisem")
    poprawnosc_lodowek <- FALSE
  }
  # if(!all(stringi::stri_detect(fridges$Klasa_zamrazarki, regex = "\\*"))){
  #   print("Przyjżyj się klasie zamrażarki, nie jestem pewien, czy jest poprawna") #TODO(Adam, doiedzieć się, jakie klasy zamrażarki są poprawne)
  #   poprawnosc_lodowek <- FALSE
  # }
  # if(class(fridges$Klasa_klimatyczna) != "character"){
  #   print("'Klasa_klimatyczna' powinna być napisem")
  #   poprawnosc_lodowek <- FALSE
  # }
  if(!all(stringi::stri_sub(fridges$Klasa_energetyczna, 1, 1) == "A")){
    print("W bazie powinny być tylko energooszczędne lodówki")
    poprawnosc_lodowek <- FALSE
  }
  
  
  return(poprawnosc_lodowek)
}


#' @param tvs dataframe with tv models
#' @return boolean
sprawdz_poprawnosc_tv <- function(tvs){
  
  poprawnosc_tv <- TRUE
  
  if(any(is.na(tvs))){
    print("W zbiorze są NA")
    return(FALSE)
  }
  
  if(!all(tvs$ID == 1:dim(tvs)[1])){
    print("ID nie są kolejnymi liczbami od 1")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(stringi::stri_sub(tvs$Nazwa, 1, 10) == rep("Telewizor ", dim(tvs)[1]))){
    print("nazwy modeli powinny zaczynać się od 'Telewizor '")
    poprawnosc_tv <- FALSE
  }
  if(!all(tvs$Cena > 0)){
    print("wszystkie modele powinny coś kosztować")
    poprawnosc_tv <- FALSE
  }
  if(!all(tvs$Pobor_mocy_tryb_czuwania_W > 0)){
    print("wszystkie modele powinny zużywać prąd w trybie czuwania")
    poprawnosc_tv <- FALSE
  }
  if(!all(tvs$Pobor_mocy_tryb_wlaczenia_W > tvs$Pobor_mocy_tryb_czuwania_W)){
    print("wszystkie modele powinny zużywać wiecej pradu w trybie dzialania niż w trybie czuwania")
    poprawnosc_tv <- FALSE
  }
  
  if(class(tvs$Zdj) != "character"){
    print("linki do zdjęć powinny być napisami")
    poprawnosc_tv <- FALSE
  }
  if(class(tvs$Firma) != "character"){
    print("firmy powinny być napisami")
    poprawnosc_tv <- FALSE
  }
  if(!all(tvs$Przekatna_ekranu_cal > 0)){
    print("Telewizor powininen mieć dodatnią przekątną ekranu")
    poprawnosc_tv <- FALSE
  }
  if(class(tvs$Standard_ekranu) != "character"){
    print("TV powinien mieć wpisaną klasę obrazu")
    poprawnosc_tv <- FALSE
  }
  if(!all(tvs$Szerokosc_ekranu_px > 0)){
    print("TV powinien mieć dodatnią szerokość ekranu")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(tvs$Wysokosc_ekranu_px > 0)){
    print("TV powinien mieć dodatnią wysokość ekranu")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(match(unique(tvs$Smart_TV), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Powinno być określone, czy Smart_TV jest obecne w telewizorze")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(match(unique(tvs$Android_TV), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Powinno być określone, czy Android_TV jest obecne w telewizorze")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(match(unique(tvs$Dla_graczy), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Powinno być określone, czy telewizor jest dla graczy")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(match(unique(tvs$Technologia_HDR_High_Dynamic_Range), c("Tak", "Nie"), nomatch = 0) > 0)){
    print("Powinno być określone, czy technologia HDR jest obecna w telewizorze")
    poprawnosc_tv <- FALSE
  }
  
  
  if(class(tvs$Zlacza_HDMI) != "numeric"){
    print("Powinna być określona liczba złącz HDMI w telewizorze")
    poprawnosc_tv <- FALSE
  }
  
  if(class(tvs$Zlacza_USB) != "numeric"){
    print("Powinna być określona liczba złącz USB w telewizorze")
    poprawnosc_tv <- FALSE
  }
  
  if(class(tvs$Kolor_obudowy) != "character"){
    print("Kolor obudowy powinien być wyrażony słownie")
    poprawnosc_tv <- FALSE
  }
  
  if(!all(match(unique(tvs$Klasa_energetyczna),
                c("G", "F", "E", "D", "C", "B", "A", "A+", "A++", "A+++"), nomatch = 0) > 0)){
    print("Klasa energetyczna powinna być wyrażona za pomocą oficjalnych oznaczeń")
    poprawnosc_tv <- FALSE
  }
  
  if(class(tvs$Czestotliwosc_odswiezania_ekranu_Hz) != "numeric"){
    print("Powinna być określona liczbowo częstotliwość odświeżania ekranu")
    poprawnosc_tv <- FALSE
  }
  
  return(poprawnosc_tv)
}