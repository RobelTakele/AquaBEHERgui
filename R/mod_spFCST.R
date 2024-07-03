#' spFCST UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_spFCST_ui <- function(id){
  ns <- NS(id)
  tagList(


    titlePanel("Seasonal Forecast"),

    sidebarLayout(
      sidebarPanel(
        title = " ",

        selectInput("FCSTvar.mapview",
                    label = h4("Choose WSC Variable:"),
                    selected = 1,
                    choices = list("onset" = 1,
                                   "cessation" = 2)),

        shinyWidgets::airDatepickerInput(
          inputId = "obsWSC_DateStart",
          label = "Observation Start Date (yyyy-MM-dd):",
          separator = " - ",
          dateFormat = "yyyy-MM-dd",
          autoClose = TRUE,
          view = c("days", "months", "years"),
          value = Sys.Date() - 7),

        shinyWidgets::airDatepickerInput(
          inputId = "obsWSC_DateEnd",
          label = "Observation End Date (yyyy-MM-dd):",
          separator = " - ",
          dateFormat = "yyyy-MM-dd",
          autoClose = TRUE,
          view = c("days", "months", "years"),
          value = Sys.Date() - 7),

        shinyWidgets::airDatepickerInput(
          inputId = "FCST_Date",
          label = "Forecast Date (yyyy-MM-dd):",
          separator = " - ",
          dateFormat = "yyyy-MM-dd",
          autoClose = TRUE,
          view = c("days", "months", "years"),
          value = Sys.Date() - 7),

      fileInput("C1.cdfInput",
                "Tercile Rain Probability (C1) : select NetCDF file to import",
                accept = ".nc"),

      fileInput("C2.cdfInput",
                "Tercile Rain Probability (C2) : select NetCDF file to import",
                accept = ".nc"),

      fileInput("C3.cdfInput",
                "Tercile Rain Probability (C3) : select NetCDF file to import",
                accept = ".nc"),

      fileInput("seasRain.cdfInput",
                "Observed Seasonal Rainfall : select NetCDF file to import",
                accept = ".nc"),

      fileInput("fcstVAR.cdfInput",
                "Observed WSC variable to forecast : select NetCDF file to import",
                accept = ".nc"),

      numericInput(inputId = "FCSTresInput",
                   label = "Set horizontal resolution in decimal degrees (\u00B0) ",
                   value = 0.0833,
                   min = 0.001,
                   max = 2,
                   width = '100%'),

      p(strong("Select output directory: ")),
      shinyFiles::shinyDirButton(id = "dirFCSTsp",
                                 label = "  Please select a folder to save data ...",
                                 icon = icon("save", lib = "glyphicon"),,
                                 class = "shinyDirectories btn-default",
                                 title = "Upload ",
                                 type = "button "),
      br(),
      verbatimTextOutput(outputId = "dirFCSTsp", placeholder	= TRUE),
      br(),

      shinyWidgets::textInputIcon(inputId = "spFCSToutFilePrefix",
                                  label = "Forecast data output file name prefix:",
                                  placeholder = "Forecast",
                                  width = '100%'),
      tags$h1(" "),
      shinyWidgets::useSweetAlert(), # /!\ needed with 'progressSweetAlert'

      div(style="position:relative; left:calc(60%);",

          shinyWidgets::actionBttn(inputId = "fcst_runButton",
                                   label = " Run Forecast",
                                   color = "primary",
                                   size = 'md',
                                   style = "jelly",
                                   #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                   icon = icon("play"),
                                   block = FALSE)
      )

    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Forecast Viewer",

       fluidPage(

           shinyjs::useShinyjs(),

             h4(verbatimTextOutput("FCSTmapTitle", placeholder = TRUE),
                align = "center"),

             tags$head(tags$style("#FCSTmapTitle{font-family: times; font-size: 20px;
                                                      font-style: bold; color: #3D3C3A;}")),

       leaflet::leafletOutput('FCSTmap',  width = "100%", height = 500),

        br(),

       column(4, offset = 0,

              shinyWidgets::textInputIcon(inputId = "FCSTmapFileN", label = "File Name:",
                                          placeholder = "Output file name", width = '100%')),

       column(3, offset = 0,
              selectInput("scene", "Select Scene:",
                          choices = c("CurrentSize", "A4Landscape", "A4Portrait")),

              shinyWidgets::actionBttn(inputId = "printFCSTmap",
                                       label = "Save Map",
                                       #     title = "Save's leaflet map to 'Downloads' directory",
                                       color = "success",
                                       size = 'md',
                                       style = "jelly",
                                       #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                       icon = icon("save"),
                                       block = TRUE)),

       br()

          )


        )

      )

    )
  )




  )
}

#' spFCST Server Functions
#'
#' @noRd
mod_spFCST_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_spFCST_ui("spFCST_1")

## To be copied in the server
# mod_spFCST_server("spFCST_1")
