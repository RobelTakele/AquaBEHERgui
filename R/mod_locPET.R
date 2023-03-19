#' locPET UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#####################################################################################################################################


#####################################################################################################################################

mod_locPET_ui <- function(id){
  ns <- NS(id)
  tagList(


    tabPanel(title = " ",

             titlePanel("Potential Evapotranspiration"),

          #   shinyjs::useShinyjs(),

             sidebarLayout(
               sidebarPanel(
                 title = " ", # "Estimate PET from spreadsheet data",

                 tags$head(
                   # this changes the size of the popovers
                   tags$style(".popover{width:500px;height:200px;}")
                 ),
                 fileInput(inputId = "PET_xlsxInput",
                           label = h4( span("Select XLSX File to Import: ",),
                                       style = "color: #4d3a7d;"
                                       # shinyBS::bsButton(inputId = "locPETq1",
                                       #                   label = "",
                                       #                   icon = icon(
                                       #                     name = NULL,
                                       #                     class = "custom_icon"
                                       #                   ),
                                       #
                                       #                   style = "default",
                                       #                   size = "extra-small",
                                       #                   type = "action",
                                       #                   block = FALSE,
                                       #                   disabled = FALSE,
                                       #                   value = FALSE)
                           ),

                           accept = ".xlsx",
                           width = '100%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL),

                 # shinyBS::bsPopover(id = "locPETq1",
                 #                    title = "Description of the data:",
                 #                    content = paste0(h5("The data must contain columns with the following parameters named accordingly; Lat, Lon, Elev, Year, Month, Day,Tmax, Tmin and additional optional parameters (Rs, Tdew, Uz).")),
                 #                    placement = "right",
                 #                    trigger = "focus", # (hover, focus, click, or manual)
                 #                    options = list(container = "body")),

                 selectInput("PETmethod", label = h4("Choose Method:"),
                             choices = list("Hargreaves-Samani" = "HS",
                                            "Priestley-Taylor" = "PT",
                                            "Penman-Monteith" = "PM"),
                             selected = 1),

                 # actionButton("PET_runButton", " Run PET", icon = icon("play"))

                 div(style="position:relative; left:calc(60%);",

                     shinyWidgets::actionBttn(inputId = "PET_runButton",
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
                     title = "Data",

                     fluidPage(

                       DT::dataTableOutput("PETtable"),

                       br(),

                       div(style="position:relative; left:calc(5%);",
                           shinyWidgets::downloadBttn(outputId = 'downloadPET',
                                                      label = "Download Data",
                                                      color = "success",
                                                      size = 'md',
                                                      style = "jelly",
                                                      icon = icon("download"),
                                                      block = FALSE)
                       ),

                       br(),

                       fluidRow(

                         br(),

                                )

                     )

                   ),
                   tabPanel(
                     title = "Plot",

                  #   shinyjs::useShinyjs(),

                     br( ),

                     div(style="position:relative; left:calc(5%);",

                         shinyWidgets::actionBttn(inputId = "PET_plotButton",
                                                  label = " Plot",
                                                  color = "success",
                                                  size = 'lg',
                                                  style = "jelly",
                                                  #   style = "color: #fff; background-color: #27ae60; border-color: #fff;",
                                                  icon = icon("chart-line"),
                                                  block = FALSE)

                     ),
                     br( ),

                     plotly::plotlyOutput("PETplot"),

                     fluidRow(

                       hr(),

                       column(4, offset = 0,
                              selectInput('PETplot_typ', 'Plot Type:', PETplot_type, PETplot_type[1]),
                              selectInput('PETplot_y', 'Y Variable:', PETplot_Ynames, PETplot_Ynames[6])
                       ),

                       column(4, offset = 1,
                              #    selectInput('PETplot_T', 'Title:', PETplot_titles, PETplot_titles[6], width = "100%"),
                              shinyWidgets::colorSelectorInput(inputId = "PETplot_color", label = "Pick a color:",
                                                               choices = col.vec, ncol = 60, selected = col.vec[1])
                       )

                     ),

                     # periscope::downloadablePlotUI(id = "PETplot",
                     #                               downloadtypes = c("png"),
                     #                               download_hovertext = "Download the plot here!",
                     #                               height = "500px",
                     #                               btn_halign = "left")
                   )

                 )
               )
             )
    )

  )
}

#' locPET Server Functions
#'
#' @noRd

mod_locPET_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


#########################################################################################################################################
#########################################################################################################################################

  })
}

## To be copied in the UI
# mod_locPET_ui("locPET_1")

## To be copied in the server
# mod_locPET_server("locPET_1")
