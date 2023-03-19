#' locWSC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_locWSC_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabPanel(title = " ",

             titlePanel("Wet-Season Calendar"),

             sidebarLayout(
               sidebarPanel(
                 title = " ",

                 fileInput("seasCal_xlsxInput", "Select XLSX File to Import", accept = ".xlsx"),

                 numericInput(inputId = "WHCinput",
                              label = "Whater Holding Capacity in (mm) ",
                              value = 100,
                              min = 10,
                              max = 200,
                              width = '100%'),


                 shinyWidgets::airDatepickerInput(
                   inputId = "onsetWindstart",
                   label = "Onset window start:",
                   value = Sys.Date() - 7),


                 shinyWidgets::airDatepickerInput(
                   inputId = "onsetWindend",
                   label = "Onset window end:",
                   value = Sys.Date() - 7),

                 shinyWidgets::airDatepickerInput(
                   inputId = "cessaWindend",
                   label = "Cessation window end:",
                   value = Sys.Date() - 7),

                 div(style="position:relative; left:calc(70%);",

                     shinyWidgets::actionBttn(inputId = "seasCal_runButton",
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
                     title = "Data",
                     DT::dataTableOutput("seasCaltable"),

                     br(),

                     div(style="position:relative; left:calc(5%);",
                         shinyWidgets::downloadBttn(outputId = 'downloadseasCal',
                                                    label = "Download WSC Data",
                                                    color = "success",
                                                    size = 'md',
                                                    style = "jelly",
                                                    icon = icon("download"),
                                                    block = FALSE)
                     )

                   ),

                   tabPanel(
                     title = "Plot",

                     br( ),

                     shinyjs::useShinyjs(),


                     br( ),

                     div(style="position:relative; left:calc(5%);",

                         shinyWidgets::actionBttn(inputId = "seasCal_plotButton",
                                                  label = " Plot WSC",
                                                  color = "success",
                                                  size = 'lg',
                                                  style = "jelly",
                                                  #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                  icon = icon("chart-line"),
                                                  block = FALSE)
                     ),

                     br( ),

                     plotly::plotlyOutput("seasCalplot"),

                     br( ),

                     fluidRow(

                       column(4, offset = 0,
                              selectInput('WSCplot_y', 'Y Variable:', WSCplot_Yvar, WSCplot_Yvar[1])
                              #  selectInput('WSCplot_ynam', 'Y-Axsis Title:', WSCplot_Ynames, WSCplot_Ynames[1])
                              #  selectInput('WSCplot_typ', 'Plot Type:', WSCplot_type, WSCplot_type[1])
                       ),

                       column(4, offset = 1,
                              shinyWidgets::textInputIcon(inputId = "WSCplot_T", label = "Title:", placeholder = "Plot Title", width = '100%'),
                              shinyWidgets::colorSelectorInput(inputId = "WSCplot_color", label = "Pick a color:",
                                                               choices = WSCcolVec, ncol = 60))

                     )

                  #   shinydlplot::downloadablePlotlyUI(id = "seasCalplot", inline = TRUE)

                   )

                 )
               )
             )
    )


  )
}

#' locWSC Server Functions
#'
#' @noRd
mod_locWSC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_locWSC_ui("locWSC_1")

## To be copied in the server
# mod_locWSC_server("locWSC_1")
