#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

#############################################################################################################################

mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
                    fluidPage(

                    shinyjs::useShinyjs(),

                      tags$button(
                        id = 'close',
                        type = "button",
                        class = "btn action-button", #icon("stop"),
                        icon("remove-sign", lib = "glyphicon"),
                        style = "font-size: 6rem; color:#ed2a0c; position: absolute; right: 50px",
                        # HTML('<i class="<FontAwesomeIcon icon="fa-solid fa-octagon-xmark" /></i>'),
                        onclick = "setTimeout(function(){window.close();},500);",  # close browser
                        h4("Close window", style = "font-size: 3rem; color:#ed2a0c;")
                      ),

                      column(7,

                             br(), br(), br(), br(),

                             imageOutput("home_img")
                      )

                      # column(12,
                      #
                      # ),

                    #  uiOutput('markdown')
                    )

  )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$home_img <- renderImage({
    #
    #   list(src = system.file("app/www/AquaBEHER.png", package = "AquaBEHERgui"),
    #        align="right",
    #        width="600")
    #
    # }, deleteFile = F)
    #
    # output$markdown <- renderUI({
    #   withMathJax(HTML(readLines(rmarkdown::render(input = system.file("app/www/Home.md", package = "AquaBEHERgui"),
    #                                                output_format = rmarkdown::html_fragment(),
    #                                                quiet = TRUE
    #   ))))
    #
    # })


  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")
