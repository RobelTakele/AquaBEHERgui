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

library(packrat)

# # ***** packrat integration

packrat::set_opts(local.repos = c("inst/app/libs"))

# ***** installs packages available in a local repository

AquaBEHERgui.packages <- c("AquaBEHER")

for (package in 1:length(AquaBEHERgui.packages)) {

  if(AquaBEHERgui.packages[package] %in% installed.packages()[,"Package"]) {
    print(paste(AquaBEHERgui.packages[package],"... installed.",sep=""),quote=FALSE)

  } else { print(paste(AquaBEHERgui.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
    packrat::install_local(AquaBEHERgui.packages[package], args = getOption("--no-staged-install"))

  }

}

# library(AquaBEHER)

##################################################################################################################
# ***** Setting Global Variables

PETplot_type <- c('scatter', 'bar')
PETplot_Ynames <- c("Tmax", "Tmin", "Rs", "Tdew", "U2",	"Eto")
PETplot_titles <- c("Daily Maximum Temperature (\u00B0C)", "Daily Mainimum Temperature (\u00B0C)",
                    "Daily Solar Radiation (MJ/m2/day)", "Daily Dew-point Temperature (\u00B0C)",
                    "Daily Wind Speed at 10-meters (m/s)","Daily Potential Evapotranspiration (mm)")

PETplot_Ynames <- c("Tmax", "Tmin", "Rs", "Tdew", "U2",	"Eto")

col.vec <- c("#0000FF","#FF0000","#00FF00","#000033","#FF00B6","#005300","#FFD300","#009FFF","#9A4D42","#00FFBE",
                      "#783FC1","steelblue","cornflowerblue","firebrick", "palegoldenrod", "violet","forestgreen", "aquamarine",
                      "dimgray", "pink", "lavender",  "magenta", "navy", "#5A5156", "#E4E1E3", "#F6222E","#FE00FA", "#16FF32",
                      "#3283FE","#FEAF16","#B00068","#1CFFCE", "#90AD1C","#2ED9FF", "#DEA0FD", "#AA0DFE","#F8A19F","#325A9B",
                      "#C4451C","#1C8356", "#85660D","#B10DA1","#FBE426", "#1CBE4F", "#FA0087", "#FC1CBF", "#F7E1A0", "#C075A6",
                      "#782AB6", "#AAF400","#BDCDFF","#822E1C","#B5EFB5", "#7ED7D1", "#1C7F93", "#D85FF7", "#683B79","#66B0FF",
                      "#209eab", "#20ab81")





##################################################################################################################
##################################################################################################################

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources( ),

    # Your application UI logic

    shiny::navbarPage(title = "AquaBEHER",
                   #   theme = shinythemes::shinytheme('cerulean'),
                    theme = "AquaBEHER_theme.css",

                      shiny::tabPanel(icon("house"),

                                      mod_Home_ui("Home_1"),

                                      uiOutput('markdown')

                      ),

    navbarMenu("PET",
              tabPanel("From Location Data", mod_locPET_ui("locPET_1")),
              tabPanel("From Gridded Data"))


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

  add_resource_path(
    "libs",
    app_sys("app/libs")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AquaBEHERgui"
    ),

    # Add here other external resources
       # shinyjs::useShinyjs()

  )
}
