#' locSWB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_locSWB_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",
             titlePanel("Soil Water Balance"),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 fileInput(inputId = "SWB_xlsxInput",
                           label = h4( span("Select XLSX File to Import: ",),
                                       style = "color: #4d3a7d;",
                                       shinyBS::bsButton(inputId = "locSWBq1",
                                                         label = "",
                                                         icon = icon( name = NULL,
                                                                      class = "custom_icon2"),
                                                         style = "default",
                                                         size = "extra-small",
                                                         type = "action",
                                                         block = FALSE,
                                                         disabled = FALSE,
                                                         value = FALSE)
                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 shinyBS::bsPopover(id = "locSWBq1",
                                    title = "Description of the required data:",
                                    content = paste0(h6("User can use the excell file exported from the PET estimation")),
                                    placement = "right",
                                    trigger = "click", # (hover, focus, click, or manual)
                                    options = list(container = "body")),

                 numericInput(inputId = "WHCinput",
                              label = "Enter Water Holding Capacity in (mm): ",
                              value = 100,
                              min = 10,
                              max = 200,
                              width = '100%'),

                 #       actionButton("SWB_runButton", "Run SWB", icon = icon("play"))

                 div(style="position:relative; left:calc(70%);",

                     shinyWidgets::actionBttn(inputId = "SWB_runButton",
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
                     title = "Data",

                     fluidPage(

                       DT::dataTableOutput("SWBtable", width = "100%", height = "auto"),

                       br(),

                       # Button
                       #  downloadButton('downloadSWB'," Download SWB Data")

                       div(style="position:relative; left:calc(5%);",
                           shinyWidgets::downloadBttn(outputId = 'downloadSWB',
                                                      label = "Download SWB Data",
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
                     shinyjs::useShinyjs(),

                     br( ),

                     div(style="position:relative; left:calc(5%);",

                         shinyWidgets::actionBttn(inputId = "SWB_plotButton",
                                                  label = " Plot",
                                                  color = "success",
                                                  size = 'lg',
                                                  style = "jelly",
                                                  #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                  icon = icon("chart-line"),
                                                  block = FALSE)

                     ),

                     br( ),

                     plotly::plotlyOutput("SWBplot"),

                     br( ),

                     fluidRow(

                       column(4, offset = 0,
                              selectInput('SWBplot_typ', 'Plot Type:', SWBplot_type, SWBplot_type[1]),

                              selectInput('SWBplot_y',
                                          #  'Y Variable:',
                                          label = h4(span("Y Variable: ",),
                                                     style = "color: #4d3a7d;",
                                                     shinyBS::bsButton(inputId = "locSWBq2",
                                                                       label = "",
                                                                       icon = icon( name = NULL,
                                                                                    class = "custom_icon3"),
                                                                       style = "default",
                                                                       size = "extra-small",
                                                                       type = "action",
                                                                       block = FALSE,
                                                                       disabled = FALSE,
                                                                       value = FALSE)
                                          ),

                                          SWBplot_Yvar, SWBplot_Yvar[1])),

                       shinyBS::bsPopover(id = "locSWBq2",
                                          title = "Description of the variables:",
                                          content = paste0(h6("TRAN: transpiration; AVAIL: Soil-moisture")),
                                          placement = "right",
                                          trigger = "click", # (hover, focus, click, or manual)
                                          options = list(container = "body")),


                       column(4, offset = 1,
                              # textInputIcon(inputId = "SWBplot_T", label = "Title:", placeholder = "Plot Title", width = '100%'),
                              shinyWidgets::colorSelectorInput(inputId = "SWBplot_color", label = "Pick a color:",
                                                               choices = col.vec, ncol = 60))

                     ),

                     column(12,
                            br( ),
                            br( ),
                            br( ),
                            br( ),
                            br( ),
                            br( ),
                            br( )
                     ),

                     shinydlplot::downloadablePlotlyUI(id = "SWBplot", inline = TRUE)

                   )

                 )

               )
             )
    )


  )
}

#' locSWB Server Functions
#'
#' @noRd
mod_locSWB_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_locSWB_ui("locSWB_1")

## To be copied in the server
# mod_locSWB_server("locSWB_1")
