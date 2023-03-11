#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyWidgets  htmltools rmarkdown plotly leaflet
#' @noRd
#'
############################################################################################################





#############################################################################################################
#############################################################################################################

app_server <- function(input, output, session) {
  # Your application server logic

  output$home_img <- renderImage({

    list(src = "inst/app/www/AquaBEHER.png",
         align="right",
         width="600")

  }, deleteFile = F)

  output$markdown <- renderUI({
    withMathJax(HTML(readLines(rmarkdown::render(input = "R/Home.md",
                                                 output_format = rmarkdown::html_fragment(),
                                                 quiet = TRUE
    ))))
  })



############################################################################################################
 # Automatically stop the shiny app when closing the browser tab

  session$onSessionEnded(stopApp)

}

############################################################################################################
############################################################################################################
############################################################################################################
