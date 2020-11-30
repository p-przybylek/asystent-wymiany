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

