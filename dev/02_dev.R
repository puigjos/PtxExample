# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "menu_module" ) # Name of the module
golem::add_module( name = "example_input" ) # Name of the module
golem::add_module( name = "example_output" ) # Name of the module
golem::add_module( name = "my_other_module" ) # Name of the module
golem::add_module( name = "GeneratePowerPoint" ) # Name of the module
golem::add_module( name = "Introduction" ) # Name of the module

## 2.2 Add dependencies

usethis::use_package("thinkr") # To call each time you need a new package
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("readxl")
usethis::use_package("officer")
usethis::use_package("flextable")
usethis::use_package("lubridate")
usethis::use_package("bizdays")
usethis::use_package("ggplot2")
usethis::use_package("ggrepel")
usethis::use_package("timevis")
usethis::use_package('shinycssloaders')
usethis::use_package('tidyr')
## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("PtxExample")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
