#' spWSC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_spWSC_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",

             titlePanel("Wet-Season Calendar"),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 shinyWidgets::airDatepickerInput(
                   inputId = "spWSC_DateStart",
                   label = "Start Date:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spWSC_DateEnd",
                   label = "End Date:",
                   value = Sys.Date() - 7),

                 fileInput("WSCswhc.cdfInput", "soilWHC: select NetCDF file to import", accept = ".nc"),

                 fileInput("WSCr.cdfInput", "R-index: select NetCDF file to import", accept = ".nc"),

                 fileInput("WSCsm.cdfInput", "Soil Moisture: select NetCDF file to import", accept = ".nc"),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spWSConsetStart",
                   label = "Onset window start:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spWSConsetEnd",
                   label = "Onset window end:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spWSCcessEnd",
                   label = "Cessation window end:",
                   value = Sys.Date() - 7),

                 numericInput(inputId = "WSCresInput",
                              label = "Set horizontal resolution in decimal degrees (\u00B0) ",
                              value = 0.0833,
                              min = 0.001,
                              max = 2,
                              width = '100%'),

                 p(strong("Select output directory: ")),
                 shinyFiles::shinyDirButton(id = "dirWSC",
                                            label = "  Please select a folder to save data ...",
                                            icon = icon("save", lib = "glyphicon"),,
                                            class = "shinyDirectories btn-default",
                                            title = "Upload ",
                                            type = "button "),
                 br(),
                 br(),

                 shinyWidgets::textInputIcon(inputId = "spWSCoutFilePrefix",
                                             label = "WSC data output file name prefix:",
                                             placeholder = "WSC",
                                             width = '100%'),


                 tags$h1(" "),
                 shinyWidgets::useSweetAlert(), # /!\ needed with 'progressSweetAlert'

                 div(style="position:relative; left:calc(60%);",

                     shinyWidgets::actionBttn(inputId = "WSCsp_runButton",
                                              label = " Run WSC",
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
                     title = "Map",

                     shinyjs::useShinyjs(),

                     fluidPage(

                       h4(verbatimTextOutput("spWSCmapTitle", placeholder = TRUE), align = "center"),
                       tags$head(tags$style("#spWSCmapTitle{font-family: times; font-size: 24px;
                                                      font-style: bold; color: #3D3C3A;}")),
                       br(),

                       leaflet::leafletOutput(outputId = 'spSeasCALmap', width = "100%", height = 500),

                       br(),

                       uiOutput(outputId = "spWSCtimeSlider"),

                       column(5, offset = 0,

                              selectInput(inputId = "spWSC.mapView",
                                          label = "Choose WSC Parameter to Display:",
                                          selected = 1,
                                          choices = list("Onset" = 1,
                                                         "Cessation" = 2,
                                                         "Duration" = 3))
                       ),

                       column(3, offset = 0,
                              shinyWidgets::textInputIcon(inputId = "spWSCmapFileN", label = "File Name:",
                                                          #  title = "Renames the leaflet map",
                                                          placeholder = "Output file name", width = '100%')),

                       column(3, offset = 0,
                              selectInput(inputId = "spWSCscene",
                                          label = "Select Scene: " ,
                                          #   title = "Selects the size mode of the leaflet map scene",
                                          choices = c("CurrentSize", "A4Landscape", "A4Portrait")),

                              shinyWidgets::actionBttn(inputId = "spWSCsave",
                                                       label = "Save Map",
                                                       #     title = "Save's leaflet map to 'Downloads' directory",
                                                       color = "success",
                                                       size = 'md',
                                                       style = "jelly",
                                                       #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                       icon = icon("save"),
                                                       block = TRUE),
                       )


                     )

                   )

                 )

               )
             )

    )

  )
}

#' spWSC Server Functions
#'
#' @noRd
mod_spWSC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_spWSC_ui("spWSC_1")

## To be copied in the server
# mod_spWSC_server("spWSC_1")
