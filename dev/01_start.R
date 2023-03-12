###############################################################################################################################
#     Building a Prod-Ready, Robust Shiny Application.
#
#     AquaBEHERgui Shiny App Development Workflow.
#
#     Copyright (C) 2022 Center of Plant Sciences, Scuola Superiore Sant???Anna (http://www.capitalisegenetics.santannapisa.it)
#
################################################################################################################################

########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Add meta data about your application
#
# golem::fill_desc(
#   pkg_name = "AquaBEHERgui",
#   pkg_title = "A shiny GUI app used to compute and visualize soil-water balance and wet-season calendar parameters",
#   pkg_description = "The app computes and integrates daily potential evapotranspiration (PET) into a daily soil-water balance model to estimate the calendar of a wet-season for agricultural applications.",
#   author_first_name = "Robel Takele",
#   author_last_name = "Matteo Dell'Acqua",
#   author_email = "takelerobel@gmail.com/matteo.dellacqua@santannapisa.it", # Your Email
#   repo_url = "https://github.com/RobelTakele/AquaBEHERgui"
# )

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files
usethis::use_ccby_license()  # ***** Free to share and adapt, must give appropriate credit.
usethis::use_readme_rmd()
# usethis::use_code_of_conduct()
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
# usethis::use_data_raw()
golem::use_recommended_deps(recommended = c("shiny","htmltools","golem", "rmarkdown", "shinyWidgets", "plotly", "leaflet", "raster"))

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Favicon ----
golem::use_favicon("inst/app/www/AquaBEHERicon.png")
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add a lib and data folder
 usethis::use_directory("inst/app/libs/", ignore = FALSE)
 usethis::use_directory("packrat", ignore = FALSE)
 usethis::use_directory("pkgdown/favicon", ignore = FALSE)



## Use git ----
usethis::use_git()

# go to dev/02_dev.R >>>>>>>>
rstudioapi::navigateToFile("dev/02_dev.R")
