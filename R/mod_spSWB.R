#' spSWB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spSWB_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",

             titlePanel("Soil Water Balance"),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 shinyWidgets::airDatepickerInput(
                   inputId = "spSWB_DateStart",
                   label = "Start Date:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "spSWB_DateEnd",
                   label = "End Date:",
                   value = Sys.Date() - 7),

                 fileInput("Rain_cdfInput", "Rain: select NetCDF file to import", accept = ".nc"),
                 fileInput("SWHC_cdfInput", "soilWHC: select NetCDF file to import", accept = ".nc"),

                 fileInput(inputId = "PET_cdfInput",
                           label = p(strong("PET: select NetCDF file to import: "),
                                     style = "color: #454545;",
                                     shinyBS::bsButton(inputId = "spSWBq1",
                                                       label = "",
                                                       icon = icon( name = NULL,
                                                                    class = "custom_icon4"),
                                                       style = "default",
                                                       size = "extra-small",
                                                       type = "action",
                                                       block = FALSE,
                                                       disabled = FALSE,
                                                       value = FALSE)
                           ),

                           accept = ".nc",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 shinyBS::bsPopover(id = "spSWBq1",
                                    title = "Description of the data:",
                                    content = paste0(h6("NetCDF file output from PET")),
                                    placement = "right",
                                    trigger = "hover", # (hover, focus, click, or manual)
                                    options = list(container = "body")),

                 numericInput(inputId = "SWBresInput",
                              label = "Set horizontal resolution in decimal degrees (\u00B0) ",
                              value = 0.0833,
                              min = 0.001,
                              max = 2,
                              width = '100%'),

                 # tags$hr(),
                 # shinyFiles::shinyDirButton("dirSWB", "Select a folder to save SWB data", "Upload"),

                 p(strong("Select output directory: ")),
                 shinyFiles::shinyDirButton(id = "dirSWB",
                                            label = "  Please select a folder to save data ...",
                                            icon = icon("save", lib = "glyphicon"),,
                                            class = "shinyDirectories btn-default",
                                            title = "Upload ",
                                            type = "button "),

                 br(),
                 br(),


                 shinyWidgets::textInputIcon(inputId = "spSWBoutFile",
                                             label = "SWB data output file name prefix:",
                                             placeholder = "SWB",
                                             width = '100%'),

                 tags$h1(" "),
                 shinyWidgets::useSweetAlert(), # /!\ needed with 'progressSweetAlert'
                 # actionButton(
                 #   inputId = "SWBsp_runButton",
                 #   label = " Run SWB !",
                 #   icon = icon("play"))

                 div(style="position:relative; left:calc(70%);",

                     shinyWidgets::actionBttn(inputId = "SWBsp_runButton",
                                              label = " Run SWB",
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

                       h4(verbatimTextOutput("spSWBmapTitle", placeholder = TRUE), align = "center"),

                       tags$head(tags$style("#spSWBmapTitle{font-family: sans-serif; font-size: 20px;
                                                      font-style: bold; color: #3D3C3A;}")),

                       leaflet::leafletOutput(outputId = 'spSWBmap', width = "100%", height = 500),

                       br(),

                       uiOutput(outputId = "spSWBtimeSlider"),

                       column(3, offset = 0,

                              selectInput("spSWB.mapView",
                                          label = "Choose SWB Parameter to Display:",
                                          choices = list("Deep Drainage" = 1,
                                                         "Transpiration" = 2,
                                                         "Surface Runoff" = 3,
                                                         "Soil Moisture" = 4,
                                                         "R-index" = 5),
                                          selected = 5)

                       ),

                       column(3, offset = 1,
                              shinyWidgets::textInputIcon(inputId = "spSWBmapFileN", label = "File Name:",
                                                          placeholder = "Output file name", width = '100%')),

                       column(2, offset = 0.5,
                              selectInput("spSWBscene", "Select Scene:",
                                          choices = c("CurrentSize", "A4Landscape", "A4Portrait")),

                              shinyWidgets::actionBttn(inputId = "spSWBsave",
                                                       label = "Save Map",
                                                       #     title = "Save's leaflet map to 'Downloads' directory",
                                                       color = "success",
                                                       size = 'md',
                                                       style = "jelly",
                                                       #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                       icon = icon("save"),
                                                       block = TRUE)),

                       #     actionButton("spSWBsave", "Save Map", icon = icon("save"))),

                       column(width = 12, offset = 0,

                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),

                       )



                     )

                   )

                 )
               )
             )
    )


  )
}

#' spSWB Server Functions
#'
#' @noRd
mod_spSWB_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_spSWB_ui("spSWB_1")

## To be copied in the server
# mod_spSWB_server("spSWB_1")
