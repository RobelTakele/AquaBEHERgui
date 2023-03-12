#' spPET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spPET_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",

             titlePanel("Potential Evapotranspiration"),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 shinyWidgets::airDatepickerInput(
                   inputId = "spPET_DateStart",
                   label = "Start Date:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spPET_DateEnd",
                   label = "End Date:",
                   value = Sys.Date() - 7),

                 selectInput("spPET_method", label = h4("Choose Method:"),
                             choices = list("Hargreaves-Samani" = "HS",
                                            "Priestley-Taylor" = "PT",
                                            "Penman-Monteith" = "PM"),
                             selected = 1),

                 fileInput("Elev_tifInput", "Elevation: select GeoTiff file to import", accept = ".tif"),

                 fileInput("Tmax_cdfInput", "Tmax: select NetCDF file to import", accept = ".nc"),

                 fileInput("Tmin_cdfInput", "Tmin: select NetCDF file to import", accept = ".nc"),


                 conditionalPanel(condition = "input.spPET_method == 'PT' || input.spPET_method == 'PM'",

                                  fileInput("SRAD_cdfInput", "SRAD: select NetCDF file to import", accept = ".nc"),

                                  fileInput("Tdew_cdfInput", "Tdew: select NetCDF file to import", accept = ".nc")

                 ),

                 conditionalPanel(condition = "input.spPET_method == 'PM'",

                                  #       fileInput("SRAD_cdfInput", "SRAD: select NetCDF file to import", accept = ".nc"),

                                  #      fileInput("Tdew_cdfInput", "Tdew: select NetCDF file to import", accept = ".nc"),

                                  fileInput("U10_cdfInput", "U10: select NetCDF file to import", accept = ".nc")

                 ),



                 numericInput(inputId = "ResInput",
                              label = "Set horizontal resolution in decimal degrees (\u00B0) ",
                              value = 0.0833,
                              min = 0.001,
                              max = 2,
                              width = '100%'),

                 p(strong("Select output directory: ")),
                 shinyFiles::shinyDirButton(id = "dir",
                                            label = "  Please select a folder to save data ...",
                                            icon = icon("folder-open ", lib = "glyphicon"),
                                            class = "shinyDirectories btn-default",
                                            title = "Upload ",
                                            type = "button "),

                 br(),
                 br(),

                 shinyWidgets::textInputIcon(inputId = "PETspOUTfile",
                                             label = "PET data output file name prefix:",
                                             placeholder = "PET",
                                             width = '100%'),


                 tags$h1(" "),
                 shinyWidgets::useSweetAlert(), # /!\ needed with 'progressSweetAlert'
                 # actionButton(
                 #   inputId = "PETsp_runButton",
                 #   label = "Run PET !",
                 #   icon = icon("play"))

                 div(style="position:relative; left:calc(70%);",

                     shinyWidgets::actionBttn(inputId = "PETsp_runButton",
                                              label = " Run PET",
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

                     fluidPage(

                       shinyjs::useShinyjs(),

                       h4(verbatimTextOutput("spPETmapTitle", placeholder = TRUE),
                          align = "center"),

                       tags$head(tags$style("#spPETmapTitle{font-family: sans-serif; font-size: 20px;
                                                      font-style: bold; color: #3D3C3A;}")),


                       leaflet::leafletOutput('spPETmap',  width = "100%", height = 500),

                       br(),

                       uiOutput(outputId = "slider"),

                       column(3, offset = 0,

                              shinyWidgets::textInputIcon(inputId = "spPETmapFileN", label = "File Name:",
                                                          placeholder = "Output file name", width = '100%')),

                       column(2, offset = 0,
                              selectInput("scene", "Select Scene:",
                                          choices = c("CurrentSize", "A4Landscape", "A4Portrait")),

                              shinyWidgets::actionBttn(inputId = "print",
                                                       label = "Save Map",
                                                       #     title = "Save's leaflet map to 'Downloads' directory",
                                                       color = "success",
                                                       size = 'md',
                                                       style = "jelly",
                                                       #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                       icon = icon("save"),
                                                       block = TRUE))

                     )

                   )

                 )

               )
             )

    )


  )
}

#' spPET Server Functions
#'
#' @noRd
mod_spPET_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_spPET_ui("spPET_1")

## To be copied in the server
# mod_spPET_server("spPET_1")
