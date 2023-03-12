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

AquaBEHER.packages <- c("devtools", "bslib", "data.table",  "doParallel", "DT", "fontawesome",
                        "fs", "ggplot2", "htmltools", "htmlwidgets", "leafem", "leaflet", "leaflet.extras",
                        "leaflet.extras2", "leaflet.multiopacity", "leaflet.opacity", "lubridate", "mapboxapi",
                        "mapview", "markdown", "ncdf4", "OpenStreetMap", "openxlsx", "periscope", "packrat", "pals", "plotly", "raster",
                        "rgdal", "rmarkdown", "shiny", "shinyalert", "shinyBS", "shinydlplot", "shinyFiles", "shinyjs", "shinythemes",
                        "shinyWidgets", "snow", "sp", "terra")

print("",quote=FALSE)
print("Checking for required R packages.",quote=FALSE)
print("******************************",quote=FALSE)

print("",quote=FALSE)
print("This is a unix-based OS, checking for additional R packages.",quote=FALSE)
print("******************************",quote=FALSE)

for (package in 1:length(AquaBEHER.packages)) {

  if(AquaBEHER.packages[package] %in% installed.packages()[,"Package"]) {

    print(paste(AquaBEHER.packages[package],"... installed.",sep=""),quote=FALSE)

  } else { print(paste(AquaBEHER.packages[package],sep=""),quote=FALSE)

    install.packages(AquaBEHER.packages[package], repos = "http://cran.us.r-project.org")}
}

print("",quote=FALSE)
print("******************************",quote=FALSE)
print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
print("Checking complete.",quote=FALSE)

###################################################################################################################

library(packrat)

 # ***** packrat integration

packrat::set_opts(local.repos = c("inst/app/libs"))

# ***** installs packages available in a local repository

AquaBEHERgui.packages <- c("AquaBEHER")

for (package in 1:length(AquaBEHERgui.packages)) {

  if(AquaBEHERgui.packages[package] %in% installed.packages()[,"Package"]) {
    print(paste(AquaBEHERgui.packages[package],"... installed.",sep=""),quote=FALSE)

  } else { print(paste(AquaBEHERgui.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
    packrat::install_local(AquaBEHERgui.packages[package])

  }

}


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


spPETmetNam <- c("Hargreaves-Samani", "Priestley-Taylor", "Penman-Monteith")
spPETmetNamS <- c("HS", "PT", "PM")

SWBplot_type <- c('scatter', 'bar')
SWBplot_Yvar <- c("Rain", "R", "AVAIL", "TRAN", "DRAIN", "RUNOFF")
locSWBplot_Ynames <-  c("Rain", "R", "AVAIL", "TRAN", "DRAIN", "RUNOFF")
locSWBplot_titles <- c("Rainfall (mm)", "R-index", "Soil-moisture (mm)", "Transpiration (mm)", "Drainage (mm)", "Runoff (mm)")

spSWBparVal <- 1:5
spSWBparNam <- c("DRAIN", "TRAN", "RUNOFF", "AVAIL", "R")
spSWBlegTitle <- c("Drainage (mm)", "Transpiration (mm)", "Runoff (mm)", "Moisture (mm)", "R-index")
spSWBcolPal <- list(rev(pals::linearl(100)), rev(pals::viridis(100)), rev(pals::ocean.haline(100)),
                    rev(pals::cubicl(100)), pals::brewer.spectral(12))

WSCplot_Yvar <- c("onset.Value", "cessation.Value", "Duration")

WSCcolVec <- c("#0000FF","#FF0000","#00FF00","#000033","#FF00B6","#005300","#FFD300","#009FFF","#9A4D42","#00FFBE",
                        "#783FC1","steelblue","cornflowerblue","firebrick", "palegoldenrod", "violet","forestgreen", "aquamarine",
                        "dimgray", "pink", "lavender",  "magenta", "navy", "#5A5156", "#E4E1E3", "#F6222E","#FE00FA", "#16FF32",
                        "#3283FE","#FEAF16","#B00068","#1CFFCE", "#90AD1C","#2ED9FF", "#DEA0FD", "#AA0DFE","#F8A19F","#325A9B",
                        "#C4451C","#1C8356", "#85660D","#B10DA1","#FBE426", "#1CBE4F", "#FA0087", "#FC1CBF", "#F7E1A0", "#C075A6",
                        "#782AB6", "#AAF400","#BDCDFF","#822E1C","#B5EFB5", "#7ED7D1", "#1C7F93", "#D85FF7", "#683B79","#66B0FF",
                        "#209eab", "#20ab81")

WSCplot_Yvar <- c("onset.Value", "cessation.Value", "Duration")
WSCplot_Ynames <- c("Days since start of onset window", "Days since start of onset window", "No. of days from onset to cessation")

spWSClegTitle <- c("Onset (DOY)", "Cessation (DOY)", "Duration (Days)")

spWSCcolPal  <- list(rev(pals::viridis(100)), rev(pals::cubicl(100)), rev(pals::cubicl(100)))

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
              tabPanel("From Gridded Data", mod_spPET_ui("spPET_1"))),

    navbarMenu("Water Balance",
               tabPanel("From Location Data", mod_locSWB_ui("locSWB_1")),
               tabPanel("From Gridded Data", mod_spSWB_ui("spSWB_1"))),

    navbarMenu("Seasonal Calendar",
               tabPanel("From Location Data", mod_locWSC_ui("locWSC_1")),
               tabPanel("From Gridded Data", mod_spWSC_ui("spWSC_1")))

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
