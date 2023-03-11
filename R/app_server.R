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

#########################################################################################################################################
#########################################################################################################################################
  # ***** PET Gridded

  # spPETmetNam <- c("Hargreaves-Samani", "Priestley-Taylor", "Penman-Monteith")
  # spPETmetNamS <- c("HS", "PT", "PM")
  #
  # locSWBplot_Ynames <-  c("Rain", "R", "AVAIL", "TRAN", "DRAIN", "RUNOFF")
  # locSWBplot_titles <- c("Rainfall (mm)", "R-index", "Soil-moisture (mm)", "Transpiration (mm)", "Drainage (mm)", "Runoff (mm)")

# ****************************************************************************************************************************************
  spElev_dataInput <- reactive({
    req(input$Elev_tifInput)
    raster::brick(input$Elev_tifInput$datapath)
  })

  spTMAX_dataInput <- reactive({
    req(input$Tmax_cdfInput)
    raster::brick(input$Tmax_cdfInput$datapath)
  })

  spTMIN_dataInput <- reactive({
    req(input$Tmin_cdfInput)
    raster::brick(input$Tmin_cdfInput$datapath)
  })

  shinyFiles::shinyDirChoose(input, 'dir', roots = c(home = '~'),
                             filetypes = c(" ", "nc", "xlsx", "tif")
  )

  global <- reactiveValues(datapath = getwd())

  dir <- reactive(input$dir)

  output$dir <- renderText({
    global$datapath
  })

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })


  # render ui for slider ----------------------------------------------------

  output$slider <- renderUI({

    sliderInput("year","PET Time Slider: ",
                min = (lubridate::as_date(input$spPET_DateStart)),
                max = (lubridate::as_date(input$spPET_DateEnd)),
                value = (lubridate::as_date(input$spPET_DateStart)),
                width = "100%",
                #      step = 3600,
                #     timezone = "+0000",
                animate = TRUE)
  })

  PETspNetCDF <- eventReactive(input$PETsp_runButton, {

    shinyWidgets::progressSweetAlert(
      session = session, id = "myprogress",
      title = h3(paste0("PET Estimation Using ", spPETmetNam[which((spPETmetNamS) == as.character(input$spPET_method))],
                        " Formulation is in Progress ....."), style = "color: #FD1C03; font-style: bold; font-family: Brush Script MT;"),
      display_pct = TRUE, value = 0, striped = TRUE, width = '50%')

    #######################################################################################################################

    SRAD.ncFile = NULL
    Tdew.ncFile = NULL
    U10.ncFile = NULL

    if (input$spPET_method == "PT" || input$spPET_method == "PM") {

      spSRAD_dataInput <- reactive({
        req(input$SRAD_cdfInput)
        raster::brick(input$SRAD_cdfInput$datapath)
      })

      spTdew_dataInput <- reactive({
        req(input$Tdew_cdfInput)
        raster::brick(input$Tdew_cdfInput$datapath)
      })

      SRAD.ncFile = spSRAD_dataInput()
      Tdew.ncFile = spTdew_dataInput()

    }

    if (input$spPET_method == "PM") {

      spU10_dataInput <- reactive({
        req(input$U10_cdfInput)
        raster::brick(input$U10_cdfInput$datapath)
      })

      U10.ncFile = spU10_dataInput()

    }


    DateStart = lubridate::as_date(input$spPET_DateStart)
    DateEnd = lubridate::as_date(input$spPET_DateEnd)
    method = input$spPET_method
    Res = input$ResInput
    out.dir = global$datapath
    out.file = input$PETspOUTfile
    Elevation.file = spElev_dataInput()
    Tmax.ncFile = spTMAX_dataInput()
    Tmin.ncFile = spTMIN_dataInput()

    date.vec <- seq.Date(from = lubridate::as_date(DateStart), to = lubridate::as_date(DateEnd), by = "day")

    tmax.ncData.r <- raster::raster(Tmax.ncFile[[1]])
    r.tmplt <- raster::raster(raster::extent(tmax.ncData.r), resolution = Res,
                              crs = raster::crs(tmax.ncData.r), vals = 1)
    r.sp <- as(r.tmplt, "SpatialGridDataFrame")

    elev.ras <- raster::resample(Elevation.file, r.tmplt, method = "ngb")
    elev.sp <- as(elev.ras, "SpatialGridDataFrame")

    tmax.ncData <- Tmax.ncFile
    tmax.nms <- names(tmax.ncData)
    tmax.nms.date.vec <- lubridate::as_date(paste0(substr(tmax.nms, 2, 5), "-", substr(tmax.nms, 7, 8), "-",
                                                   substr(tmax.nms, 10, 11)))
    startdate.index <- which(tmax.nms.date.vec == DateStart)
    enddate.index <- which(tmax.nms.date.vec == DateEnd)
    data.tmax <- tmax.ncData[[startdate.index:enddate.index]]

    tmin.ncData <- Tmin.ncFile
    tmin.nms <- names(tmin.ncData)
    tmin.nms.date.vec <- lubridate::as_date(paste0(substr(tmin.nms, 2, 5), "-", substr(tmin.nms, 7, 8), "-",
                                                   substr(tmin.nms, 10, 11)))
    startdate.index <- which(tmin.nms.date.vec == DateStart)
    enddate.index <- which(tmin.nms.date.vec == DateEnd)
    data.tmin <- tmin.ncData[[startdate.index:enddate.index]]

    if (!is.null(SRAD.ncFile)) {

      srad.ncData <- SRAD.ncFile
      srad.nms <- names(srad.ncData)
      srad.nms.date.vec <- lubridate::as_date(paste0(substr(srad.nms, 2, 5), "-", substr(srad.nms, 7, 8), "-",
                                                     substr(srad.nms, 10, 11)))
      startdate.index <- which(srad.nms.date.vec == DateStart)
      enddate.index <- which(srad.nms.date.vec == DateEnd)
      data.srad <- srad.ncData[[startdate.index:enddate.index]]
    }

    if (!is.null(Tdew.ncFile)) {

      tdew.ncData <- Tdew.ncFile
      tdew.nms <- names(tdew.ncData)
      tdew.nms.date.vec <- lubridate::as_date(paste0(substr(tdew.nms, 2, 5), "-", substr(tdew.nms, 7, 8), "-",
                                                     substr(tdew.nms, 10, 11)))
      startdate.index <- which(tdew.nms.date.vec == DateStart)
      enddate.index <- which(tdew.nms.date.vec == DateEnd)
      data.tdew <- tdew.ncData[[startdate.index:enddate.index]]

    }

    if (!is.null(U10.ncFile)) {

      u10.ncData <- U10.ncFile
      u10.nms <- names(u10.ncData)
      u10.nms.date.vec <- lubridate::as_date(paste0(substr(u10.nms, 2, 5), "-", substr(u10.nms, 7, 8), "-",
                                                    substr(u10.nms, 10, 11)))
      startdate.index <- which(u10.nms.date.vec == DateStart)
      enddate.index <- which(u10.nms.date.vec == DateEnd)
      data.u10 <- u10.ncData[[startdate.index:enddate.index]]

    }

    # *********************************************************************************************************************

    doParallel::registerDoParallel(cores = 4)

    tmax.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {
      as(raster::resample(data.tmax[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.tmax.sp <- snow::docall(sp::cbind.Spatial, tmax.sp.list)

    tmin.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {
      as(raster::resample(data.tmin[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.tmin.sp <- snow::docall(sp::cbind.Spatial, tmin.sp.list)


    tmin.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {
      as(raster::resample(data.tmin[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.tmin.sp <- snow::docall(sp::cbind.Spatial, tmin.sp.list)

    if (!is.null(SRAD.ncFile)) {

      srad.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {
        as(raster::resample(data.srad[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
      }

      data.srad.sp <- snow::docall(sp::cbind.Spatial, srad.sp.list)

    }

    if (!is.null(Tdew.ncFile)) {

      tdew.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {

        as(raster::resample(data.tdew[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
      }

      data.tdew.sp <- snow::docall(sp::cbind.Spatial, tdew.sp.list)

    }

    if (!is.null(U10.ncFile)) {


      u10.sp.list <- foreach(d = 1 : length(date.vec)) %dopar% {

        as(raster::resample(data.u10[[d]], r.tmplt, method = "bilinear"), "SpatialGridDataFrame")
      }

      data.u10.sp <- snow::docall(sp::cbind.Spatial, u10.sp.list)


    }

    # **********************************************************************************************************************

    PET.sp <- r.sp
    PET.sp@data <- data.frame(matrix(NA, nrow = nrow(r.sp@data), ncol = length(date.vec)))

    for (grd in 1:nrow(PET.sp@data)) {

      if (method == "HS") {

        data <- data.frame(Lat = sp::coordinates(PET.sp)[,2][grd],
                           Lon = sp::coordinates(PET.sp)[,1][grd],
                           Elev = elev.sp@data[grd, ],
                           Year = lubridate::year(date.vec),
                           Month = lubridate::month(date.vec),
                           Day = lubridate::day(date.vec),
                           Tmax = as.numeric(data.tmax.sp@data[grd, ]),
                           Tmin = as.numeric(data.tmin.sp@data[grd, ]))

      } else if (method == "PT") {


        data <- data.frame(Lat = sp::coordinates(PET.sp)[,2][grd],
                           Lon = sp::coordinates(PET.sp)[,1][grd],
                           Elev = elev.sp@data[grd, ],
                           Year = lubridate::year(date.vec),
                           Month = lubridate::month(date.vec),
                           Day = lubridate::day(date.vec),
                           Tmax = as.numeric(data.tmax.sp@data[grd, ]),
                           Tmin = as.numeric(data.tmin.sp@data[grd, ]),
                           Rs = as.numeric(data.srad.sp@data[grd, ]),
                           Tdew = as.numeric(data.tdew.sp@data[grd, ]))

      } else if (method == "PM") {

        data <- data.frame(Lat = sp::coordinates(PET.sp)[,2][grd],
                           Lon = sp::coordinates(PET.sp)[,1][grd],
                           Elev = elev.sp@data[grd, ],
                           Year = lubridate::year(date.vec),
                           Month = lubridate::month(date.vec),
                           Day = lubridate::day(date.vec),
                           Tmax = as.numeric(data.tmax.sp@data[grd, ]),
                           Tmin = as.numeric(data.tmin.sp@data[grd, ]),
                           Rs = as.numeric(data.srad.sp@data[grd, ]),
                           Tdew = as.numeric(data.tdew.sp@data[grd, ]),
                           Uz = as.numeric(data.u10.sp@data[grd, ]))
      }

      PET.grd <- AquaBEHER::calcEto(data = data, method = method, crop = "short", Zh = 10)
      PET.sp@data[grd,] <- as.numeric(PET.grd$ET.Daily)

      Sys.sleep(0.1)
      shinyWidgets::updateProgressBar(
        session = session,
        id = "myprogress",
        value = grd,
        total = nrow(PET.sp@data))
    }

    PET.rasBRK <- terra::rast(raster::brick(PET.sp))
    terra::time(PET.rasBRK) <- lubridate::as_date(date.vec)

    terra::writeCDF(PET.rasBRK,
                    filename = paste0(out.dir, "/",  out.file, "_", Res, "_", method, "_", DateStart,"_T_", DateEnd, ".nc"),
                    varname = "PET",
                    longname = paste0("Potential Evapotranspiration [", method, "]"),
                    unit = 'mm',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" Calculation completed !",
      type = "success"
    )

    PET.rasBRK

  })


  output$spPETmap =  leaflet::renderLeaflet({

    leaflet::leaflet() %>%
      mapboxapi::addMapboxTiles(style_id = "satellite-streets-v12", username = "mapbox",
                                access_token = "pk.eyJ1Ijoicm9iZWx0YWtlbGUiLCJhIjoiY2xkb2o4NmRtMDEzcjNubHBkenMycnhiaSJ9.UkdfagqGIy7WjMGXtlT1mQ") %>%

      leaflet.multiopacity::addOpacityControls(group = "PETlayers",
                                               collapsed = FALSE,
                                               position = "bottomleft",
                                               size = "m",
                                               title = "PET Opacity Control:",
                                               renderOnLayerAdd = TRUE) %>%

      leaflet::addMiniMap(position = "bottomright",
                          width = 150,
                          height = 150) %>%
      leaflet::setView(lng = 38, lat = -14, zoom = 4)
  })


  spPETmapLeaf <- observeEvent(input$PETsp_runButton, {

    PET.mapDat <- PETspNetCDF()
    PET.mapDat.nms <- names(PET.mapDat)
    PET.mapDat.nms.dateVec <- lubridate::as_date(paste0(substr(PET.mapDat.nms, 2, 5), "-",
                                                        substr(PET.mapDat.nms, 7, 8), "-", substr(PET.mapDat.nms, 10, 11)))
    PETmap.index <- 1

    PET.map_leaflet <- leaflet::projectRasterForLeaflet(PET.mapDat[[PETmap.index]], method = "bilinear")

    PETcolorPal <- leaflet::colorNumeric(pals::brewer.spectral(12), raster::values( PET.map_leaflet),
                                         na.color = "transparent")

    leaflet::leafletProxy("spPETmap", session) %>%

      leaflet::addRasterImage(PET.map_leaflet, colors =  PETcolorPal, opacity = 0.8,
                              group = "PETlayers", layerId = "PETlayers") %>%

      leaflet::addLegend(pal = PETcolorPal, values = raster::values(PET.map_leaflet), opacity = 1,
                         title = "PET (mm)", position = "topright")

  })


  observeEvent(input$spPET_method, {

    spPETmetdNamIndex <- which((spPETmetNamS) == as.character(input$spPET_method))

    output$spPETmapTitle <- renderText({paste0(spPETmetNam[spPETmetdNamIndex],
                                               " Potential Evapotranspiration (PET): ",
                                               paste(input$year))})
  })

  observeEvent(input$year,{

    PET.mapDat <- PETspNetCDF()
    DateStart = lubridate::as_date(input$spPET_DateStart)
    DateEnd = lubridate::as_date(input$spPET_DateEnd)

    date.vec <- seq.Date(from = lubridate::as_date(DateStart), to = lubridate::as_date(DateEnd), by = "day")

    PETmap.index <- as.numeric(which(date.vec == lubridate::as_date(input$year)))    #1

    PET.map_leaflet <- leaflet::projectRasterForLeaflet(PET.mapDat[[PETmap.index]], method = "bilinear")

    PETcolorPal <- leaflet::colorNumeric(pals::brewer.spectral(12), raster::values( PET.map_leaflet),
                                         na.color = "transparent")

    leaflet::leafletProxy("spPETmap", session) %>% leaflet::clearControls() %>%

      leaflet::addRasterImage(PET.map_leaflet, colors =  PETcolorPal, opacity = 0.8,
                              group = "PETlayers", layerId = "PETlayers") %>%

      leaflet::addLegend(pal = PETcolorPal, values = raster::values(PET.map_leaflet),
                         title = "PET (mm)", position = "topright",
                         group = "PETlayers", layerId = "PETlayers") %>%

      leaflet.extras2::addEasyprint(options = leaflet.extras2::easyprintOptions(exportOnly = TRUE,
                                                                                hidden = TRUE,
                                                                                hideControlContainer = FALSE))
  })

  observeEvent(input$print, {
    leaflet::leafletProxy("spPETmap", session) %>%
      leaflet.extras2::easyprintMap(sizeModes = input$scene, filename = input$spPETmapFileN)
  })

#########################################################################################################################################
#########################################################################################################################################

############################################################################################################
 # Automatically stop the shiny app when closing the browser tab

  session$onSessionEnded(stopApp)

}

############################################################################################################
############################################################################################################
############################################################################################################
