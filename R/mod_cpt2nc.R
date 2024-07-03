#' cpt2nc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cpt2nc_ui <- function(id){
  ns <- NS(id)
  tagList(


    tabPanel(title = " ",

             titlePanel("Convert CPT file to NetCDF"),

             #   shinyjs::useShinyjs(),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 tags$head(
                   # this changes the size of the popovers
                   tags$style(".popover{width:500px;height:200px;}")
                 ),


                 fileInput(inputId = "cpt2nc_xlsxInput",
                           label = h4( span("Select CPT Tercile Probability File to Import: ",),
                                       style = "color: #4d3a7d;"

                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 numericInput(inputId = "LONGlengthInput",
                              label = "Input the number of longitudes",
                              value = 152,
                              min = 1,
                              max = 10000,
                              width = '100%'),

                 numericInput(inputId = "LATlengthInput",
                              label = "Input the number of latitudes",
                              value = 234,
                              min = 1,
                              max = 10000,
                              width = '100%'),

                 shinyWidgets::airDatepickerInput(
                   inputId = "cpt2nc_date",
                   label = "Forecast Start Date (yyyy-MM-dd):",
                   separator = " - ",
                   dateFormat = "yyyy-MM-dd",
                   autoClose = TRUE,
                   view = c("days", "months", "years"),
                   value = Sys.Date() - 7),


                 p(strong("Select NetCDF output directory: ")),
                 shinyFiles::shinyDirButton(id = "dirCDF",
                                            label = "  Please select a folder to save NetCDF data ...",
                                            icon = icon("save", lib = "glyphicon"),,
                                            class = "shinyDirectories btn-default",
                                            title = "Upload ",
                                            type = "button "),
                 br(),
                 verbatimTextOutput(outputId = "dirCDF", placeholder	= TRUE),
                 br(),

                 shinyWidgets::textInputIcon(inputId = "cdfFilePrefix",
                                             label = "NetCDF data output file name prefix:",
                                             placeholder = "NetCDF",
                                             width = '100%'),

                 div(style="position:relative; left:calc(70%);",

                     shinyWidgets::actionBttn(inputId = "cpt2nc_runButton",
                                              label = " Convert ",
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
                   title = "Data Viewer",

                   shinyjs::useShinyjs(),

                   fluidPage(

                     br(),

                     leaflet::leafletOutput(outputId = 'rainTERmap', width = "100%", height = 500),

                     br(),


                   )
                 )
               )
             )

         )
    )




  )
}

#' cpt2nc Server Functions
#'
#' @noRd
mod_cpt2nc_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_cpt2nc_ui("cpt2nc_1")

## To be copied in the server
# mod_cpt2nc_server("cpt2nc_1")
