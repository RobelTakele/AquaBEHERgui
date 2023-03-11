#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyWidgets  htmltools rmarkdown plotly leaflet
#' @noRd
#'
############################################################################################################

# PETplot_titles <- c("Daily Maximum Temperature (0C)", "Daily Mainimum Temperature (0C)",
#                     "Daily Solar Radiation (MJ/m2/day)", "Daily Dew-point Temperature (0C)",
#                     "Daily Wind Speed at 10-meters (m/s)","Daily Potential Evapotranspiration (mm)")

# PETplot_Ynames <- c("Tmax", "Tmin", "Rs", "Tdew", "U2",	"Eto")



#############################################################################################################
#############################################################################################################

app_server <- function(input, output, session) {
  # Your application server logic

  output$home_img <- renderImage({

    list(src = system.file("app/www/AquaBEHER.png", package = "AquaBEHERgui"),
         align="right",
         width="600")

  }, deleteFile = F)

  output$markdown <- renderUI({
    withMathJax(HTML(readLines(rmarkdown::render(input = system.file("app/www/Home.md", package = "AquaBEHERgui"),
                                                 output_format = rmarkdown::html_fragment(),
                                                 quiet = TRUE
    ))))

    })

#################################################################################################################
#################################################################################################################
  # ***** PET

  PET_dataInput <- reactive({
    req(input$PET_xlsxInput)
    openxlsx::read.xlsx(input$PET_xlsxInput$datapath, sheet = 1)
  })

  PET_table <- eventReactive(input$PET_runButton, {

    PETdata <- PET_dataInput()
    PET <- AquaBEHER::calcEto(data = PETdata, method = input$PETmethod, crop = "short", Zh = 10)
    PETdata$Eto <- round(PET$ET.Daily, 2)
    PETdata

  })

  output$PETtable <- DT::renderDataTable(DT::datatable({

    PET_table()

  }))

  output$downloadPET <- downloadHandler(
    filename = function() {
      paste0("PET_", input$PETmethod, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(PET_table(), file)
    }
  )

  # *********************************************************************************************************************************

  plot_PET <- eventReactive(input$PET_plotButton, {

    x.date <- lubridate::as_date(paste0(PET_table()$Year, "-", PET_table()$Month, "-", PET_table()$Day))
    pet.dF <- PET_table()

    if (input$PETplot_typ == 'scatter') {
      locPETplt <- plotly::plot_ly(data = pet.dF, type = input$PETplot_typ,  mode = 'line', x = x.date,  y =  pet.dF[,input$PETplot_y],
                                   color = I(input$PETplot_color), alpha = 1)
    }else {
      locPETplt <- plotly::plot_ly(data = pet.dF, type = input$PETplot_typ, x = x.date,  y =  pet.dF[,input$PETplot_y],
                                   color = I(input$PETplot_color), alpha = 1)
    }

    locPETplt %>% plotly::config(displaylogo = FALSE,
                                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                             filename= paste0(input$PETplot_y),
                                                             height= NULL,
                                                             width= NULL,
                                                             scale= 1 ))  %>%
      plotly::layout(xaxis = list(
        type = 'date',
        tickformat = "%d-%b-%Y",
        showticklabels='True',
        gridcolor = 'ffff',

        rangeselector = list(
          buttons = list(
            list(
              count = 3,
              label = "3 mo",
              step = "month",
              stepmode = "backward"),

            list(
              count = 6,
              label = "6 mo",
              step = "month",
              stepmode = "backward"),

            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward"),

            list(
              count = 1,
              label = "YTD",
              step = "year",
              stepmode = "todate"),
            list(step = "all"))),

        rangeslider = list(type = "date")),

        yaxis = list (title = PETplot_titles[which(input$PETplot_y == PETplot_Ynames)],
                      gridcolor = 'ffff',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2.5),
        plot_bgcolor='#e5f0fc')

  })


  output$PETplot <- plotly::renderPlotly({

    plot_PET()

  })


############################################################################################################
 # Automatically stop the shiny app when closing the browser tab

  session$onSessionEnded(stopApp)

}

############################################################################################################
############################################################################################################
############################################################################################################
