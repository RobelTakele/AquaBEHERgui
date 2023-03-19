#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
##################################################################################################################
##################################################################################################################
#######################################################################################################################
# ***** Declaring Functions

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# source("startUP.R")

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
spSWBcolPal <- list(rev(c("#440154","#462777","#3D4988","#30678D","#25818E","#1F9D87","#36B677","#6DCC57","#B3DC2B","#FDE725")),
                    rev(c("#440154","#462777","#3D4988","#30678D","#25818E","#1F9D87","#36B677","#6DCC57","#B3DC2B","#FDE725")),
                    rev(c("#780085","#7F2FD1","#686DF9","#4C9ED9","#3FC29F","#53DA60","#85EB50","#C1EC58","#E4D05C","#F9965B")),
                    rev(c("#780085","#7F2FD1","#686DF9","#4C9ED9","#3FC29F","#53DA60","#85EB50","#C1EC58","#E4D05C","#F9965B")),
                    c("#9E0142","#D0384D","#EE6445","#FA9C58","#FDCD7B","#FEF0A7","#F3FAAD","#D0EC9C","#98D5A4","#5CB7A9","#3682BA","#5E4FA2"))

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

spWSCcolPal  <- list(rev(c("#440154","#462777","#3D4988","#30678D","#25818E","#1F9D87","#36B677","#6DCC57","#B3DC2B","#FDE725")),
                     rev(c("#780085","#7F2FD1","#686DF9","#4C9ED9","#3FC29F","#53DA60","#85EB50","#C1EC58","#E4D05C","#F9965B")),
                     rev(c("#780085","#7F2FD1","#686DF9","#4C9ED9","#3FC29F","#53DA60","#85EB50","#C1EC58","#E4D05C","#F9965B")))

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

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AquaBEHERgui"
    ),

    # Add here other external resources
        shinyjs::useShinyjs()

  )
}
