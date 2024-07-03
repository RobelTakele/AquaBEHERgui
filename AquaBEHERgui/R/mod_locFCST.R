#' locFCST UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_locFCST_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",

             titlePanel("Seasonal Forecast"),

             #   shinyjs::useShinyjs(),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 tags$head(
                   # this changes the size of the popovers
                   tags$style(".popover{width:500px;height:200px;}")
                 ),

                 fileInput(inputId = "rainTerc_xlsxInput",
                           label = h4( span("Select Tercile Probability File to Import: ",),
                                       style = "color: #4d3a7d;"

                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 fileInput(inputId = "sesRain_xlsxInput",
                           label = h4( span("Select Seasonal Rain File to Import: ",),
                                       style = "color: #4d3a7d;"

                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 fileInput(inputId = "fcstVarDF_xlsxInput",
                           label = h4( span("Select WSC Variable File to Import: ",),
                                       style = "color: #4d3a7d;"

                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 selectInput("FCSTvar", label = h4("Choose WSC Variable:"),
                             choices = list("Onset" = "onset",
                                            "Cessation" = "cessation"),
                             selected = 1),

                 div(style="position:relative; left:calc(60%);",

                     shinyWidgets::actionBttn(inputId = "FCST_runButton",
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
                     title = "Data",

                     fluidPage(

                  DT::dataTableOutput("seasFCSTtable"),

                       br(),

                       div(style="position:relative; left:calc(5%);",
                           shinyWidgets::downloadBttn(outputId = 'downloadFCST',
                                                      label = "Download Forecast Data",
                                                      color = "success",
                                                      size = 'md',
                                                      style = "jelly",
                                                      icon = icon("download"),
                                                      block = FALSE)
                       )
                     )
                   ),

                  tabPanel(
                    title = "Plot",

                    br( ),

                    shinyjs::useShinyjs(),

                    br( ),

                    div(style="position:relative; left:calc(5%);",

                        shinyWidgets::actionBttn(inputId = "seasFCST_plotButton",
                                                 label = " Plot Forecast",
                                                 color = "success",
                                                 size = 'lg',
                                                 style = "jelly",
                                                 #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                 icon = icon("chart-line"),
                                                 block = FALSE)
                    ),

                    br( ),

                    plotly::plotlyOutput("seasFCSTplot"),

                    br( ),

                  )

                   )

                 )
               )
             )

  )
}

#' locFCST Server Functions
#'
#' @noRd
mod_locFCST_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_locFCST_ui("locFCST_1")

## To be copied in the server
# mod_locFCST_server("locFCST_1")
