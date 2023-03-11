#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
##################################################################################################################
##################################################################################################################
# ------------------------------------------------
# checks that the required packages are installed for running AquaBEHERgui.
#
# ------------------------------------------------

AquaBEHER.packages <- c("devtools","htmltools", "htmlwidgets", "shinyjs", "shinythemes", "shinyWidgets", "rmarkdown", "markdown",
                        "packrat")

print("",quote=FALSE)
print("Checking for required R packages.",quote=FALSE)
print("******************************",quote=FALSE)

print("",quote=FALSE)
print("This is a unix-based OS, checking for additional R packages.",quote=FALSE)
print("******************************",quote=FALSE)

for (package in 1:length(AquaBEHER.packages)) {

  if(AquaBEHER.packages[package] %in% installed.packages()[,"Package"]) {

    print(paste(AquaBEHER.packages[package],"... installed.",sep=""),quote=FALSE)

  } else { print(paste(AquaBEHER.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)

    install.packages(AquaBEHER.packages[package], repos = "http://cran.us.r-project.org")}
}

print("",quote=FALSE)
print("******************************",quote=FALSE)
print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
print("Checking complete.",quote=FALSE)

###################################################################################################################
#
# library(packrat)
#
# # ***** packrat integration
#
# packrat::set_opts(local.repos = c("inst/app/libs"))
#
# # ***** installs packages available in a local repository
#
# AquaBEHERgui.packages <- c("AquaBEHER")
#
# for (package in 1:length(AquaBEHERgui.packages)) {
#
#   if(AquaBEHERgui.packages[package] %in% installed.packages()[,"Package"]) {
#     print(paste(AquaBEHERgui.packages[package],"... installed.",sep=""),quote=FALSE)
#
#   } else { print(paste(AquaBEHERgui.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
#     packrat::install_local(AquaBEHERgui.packages[package])
#
#   }
#
# }

# library(AquaBEHER)


##################################################################################################################

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic

    shiny::navbarPage(title = "AquaBEHER",
                   #   theme = shinythemes::shinytheme('cerulean'),
                    theme = "AquaBEHER_theme.css",

                      shiny::tabPanel(icon("house"),

                                      mod_Home_ui("Home_1")
                      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AquaBEHERgui"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
