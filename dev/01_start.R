# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "asystentWymiany", # The Name of the package containing the App 
  pkg_title = "Asystent Wymiany", # The Title of the package containing the App 
  pkg_description = "Prototyp asystenta wymiany sprzetu RTV i AGD, czyli rozszeżenie aplikacji dla klienta NAATU.", # The Description of the package containing the App 
  author_first_name = "Przemyslaw", # Your First Name
  author_last_name = "Chojecki", # Your Last Name
  author_email = "p.chojecki@student.mini.pw.edu.pl", # Your Email
  repo_url = "https://github.com/p-przybylek/asystent-wymiany" # The URL of the GitHub Repo (optional) 
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( name = "Golem User" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
#usethis::use_lifecycle_badge( "Experimental" ) # mi nie dziala TODO(ADAM; niech zadziala)
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one) # nie chcemy
#golem::remove_favicon()
#golem::use_favicon() # path = "path/to/ico". Can be an online file. 

## Add helper functions ---- # nie chcemy
#golem::use_utils_ui()
#golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

