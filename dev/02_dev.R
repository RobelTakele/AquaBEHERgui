# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################
# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add internal datasets ----
usethis::use_data_raw(name = "rcn", open = FALSE)
rcn <- raster::raster("../ExampleData/af_rcn_5km.tif")
save(rcn, file="data-raw/rcn.rda")
usethis::use_data(rcn, internal = TRUE, overwrite = TRUE)
usethis::use_data(rcn, internal = FALSE, overwrite = TRUE)

#######################################################################################
## ***** Add modules ----
## Create a module infrastructure in R/

golem::add_module(name = 'Home', with_test = TRUE)
golem::add_module(name = 'markdownHome', with_test = TRUE)
golem::add_module(name = 'locPET', with_test = TRUE)
golem::add_module(name = 'spPET', with_test = TRUE)
golem::add_module(name = 'locSWB', with_test = TRUE)
golem::add_module(name = 'spSWB', with_test = TRUE)
golem::add_module(name = 'locWSC', with_test = TRUE)
golem::add_module(name = 'spWSC', with_test = TRUE)

## External resources
golem::use_internal_file("../Resourses/Home.md",  open = TRUE)

## Add helper functions ----
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)



## Tests ----
## Add one line by test you want to create
usethis::use_test("AquaBEHERgui")

## CI ----
usethis::use_github()

# GitHub Actions
usethis::use_github_action("pkgdown")
usethis::use_pkgdown_github_pages()

# Chose one of the three
usethis::use_github_action_check_standard()

# Add a file for Shiny Server
golem::add_shinyserver_file()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

# Jenkins
usethis::use_jenkins()

###################################################################################################
###################################################################################################



## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

# Documentation

## Vignette ----
usethis::use_vignette("AquaBEHERgui")
devtools::build_vignettes()

# # Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()

# # AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()

# Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
