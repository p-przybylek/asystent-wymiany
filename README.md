
<!-- README.md is generated from README.Rmd. Please edit that file -->
Asystent wymiany sprzętu RTF i AGD
==================================

<!-- badges: start -->
<!-- badges: end -->
Rozwiązanie przeznaczone dla użytkownika końcowego terminala PLISZKA.

Aplikacja ma za zadanie, na podstawie danych o zużyciu energii elektrycznej przez poszczególne urządzenia (dostarczanych przez sam terminal) oraz bazy modeli poszczególnych typów urządzeń RTV i AGD, przedstawiać użytkownikowi najbardziej dla niego opłacalne wymiany odbiorników prądu na bardziej energooszczędne modele. W tym celu, ma ona wizualizować koszt i opłacalność poszczególnych wymian, w zależności od stawek dostawcy energii, obecnego modelu i charakterystyki użytkowania danego urządzenia.

How to work in here
-------------------

### Odpalanie apki

1.  Otwierasz plik `asystent-wymiany.Rproj`.
2.  W prawym górym rogu RStudio wybierasz zakładkę "Build".
3.  Klikasz "Install and Restart".
4.  Czekasz 15-20 sekund.
5.  Wpisujesz komendę `asystentWymiany::run_app()`.

UWAGA: Teoretycznie można wejść w `dev/run_dev.R` i to odpalić i powinno działać, ale mi osobiście on nie czyta `CSS`a, więc robię tak, jak jest napisane powyżej.

### Gdzie co jest i czym jest

[*R packages* by H. Wickham](https://r-pkgs.org/)

1.  Kody R są w folderze `R`.
2.  Dane `.Rda` i `.csv` będą w folderze `data`. Nie wiem, czy będziemy je tam segregować w subfolderach. Mim zdaniem nie trzeba.
3.  W `dev` są kody do pracy z GOLEMem.
4.  W `inst/app/www` są `CSS`y i `JS`y.
5.  W `man` są automatycznie generowane pliki pomocy. Nie zmieniajcie ich ręcznie - i tak się nadpiszą.
6.  `README.md` jest generowany z `README.Rmd`. Jak już to edytujcie ten drugi.
7.  W `tests/testthat` są pliki z testami jednostkowymi.

### Pisanie w ui/server

Jeśli używacie jakiejś funkcji z jakiegoś pakietu, np `shinydashboard::dashboardPage`, to piszcie to z `::`, bo inaczej czasem się psuje.

#### Testy jednostkowe

Najlepiej jak najwięcej kodu zamykać w funkcje zdefiniowane w `R/interface_elements.R` i napisać do nich testy jednostkowe w `tests/testthat/test-interface_elements.R`. Okulewicz będize zadowolony a i my pewnie szybciej błędy znajdziemy.

Aby je odpalić wywołujesz komendę `devtools::test()` gdy ma się `getwd()` w folderze z pakietem.

### Check

Obok przycisku "Install and Restart" jest guzik "Check". Jak go się kliknie to R sprawdzi cały pakiet, czy jest poprawny. Nie tylko wywoła testy jednostkowe, ale też sprawdzi, czy inne rzeczy działają, czy mamy literówki(albo polskie słowa xd) w kodzie, błędy albo braki w dokumentacji i takie tam. Trwa około 3 minuty.

### Dokumentacja

Nad każdą funkcją piszemy komentarze dokumentujące jej działanie, przyjmowane parametry, zwracaną wartość, informację, że ją eksportujemy(wydaje mi się, żeby zrobić tak ze wszystkimi funkcjami, to będzie łatwo się dostać do dokumentacji xd), jakie pakiety są u niej używane oraz przykład użycia.

Przykładem ładnie odokumentowanej funkcji jest `filter_by_attr` w `R/server_utilis.R`. On jest napisany po angielsku, ale wydaje mi się, że lepiej byłoby to pisać po polsku. Wydaje mi się to łatwiejsze w zrozumieniu.

Code of Conduct
---------------

Please note that the asystentWymiany project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
