#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyWidgets  htmltools rmarkdown plotly leaflet foreach
#' @importFrom methods as
#' @noRd
#'
############################################################################################################

globalVariables(c("d"))

##################################################################################################################

# library(AquaBEHER)

suppressPackageStartupMessages({

library(terra)
library(sp)

})
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
  # ***** SWB

  SWB_dataInput <- reactive({
    req(input$SWB_xlsxInput)
    openxlsx::read.xlsx(input$SWB_xlsxInput$datapath, sheet = 1)
  })

  SWB_table <- eventReactive(input$SWB_runButton, {

    SWBdat <- SWB_dataInput()
    SWBdata <- AquaBEHER::calcWatBal(data = SWBdat, soilWHC = input$WHCinput)
    SWBdata$R <- round(SWBdata$R, 2)
    SWBdata$AVAIL <- round(SWBdata$AVAIL, 2)
    SWBdata$TRAN <- round(SWBdata$TRAN, 2)
    SWBdata$DRAIN <- round(SWBdata$DRAIN, 2)
    SWBdata$RUNOFF <- round(SWBdata$RUNOFF, 2)
    SWBdata

  })

  output$SWBtable <- DT::renderDataTable(DT::datatable({

    SWB_table()

  }))

  output$downloadSWB <- downloadHandler(
    filename = function() {
      paste0("SWB_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(SWB_table(), file)
    }
  )

  plot_SWB <- eventReactive(input$SWB_plotButton, {

    SWB.x.date <- as.Date(paste0(SWB_table()$Year, "-", SWB_table()$Month, "-", SWB_table()$Day))
    locSWB.dF <- SWB_table()

    if (input$SWBplot_typ == 'scatter') {

      locSWBplt <- plotly::plot_ly(data = locSWB.dF, type = input$SWBplot_typ, x = SWB.x.date, mode = 'lines',
                                   y = locSWB.dF[ ,input$SWBplot_y], color = I(input$SWBplot_color), alpha = 1)
    }else {

      locSWBplt <- plotly::plot_ly(data = locSWB.dF, type = input$SWBplot_typ, x = SWB.x.date,
                                   y = locSWB.dF[ ,input$SWBplot_y], color = I(input$SWBplot_color), alpha = 1)
    }


    locSWBplt %>% plotly::config(displaylogo = FALSE,
                                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                             filename= paste0(input$SWBplot_y),
                                                             height= NULL,
                                                             width= NULL,
                                                             scale= 1 ))   %>%

      plotly::layout(title = input$SWBplot_T, font = list(size = 12),
                     xaxis = list(
                       zerolinewidth = 2,
                       type = 'date',
                       tickformat = "%d-%b-%Y",
                       showticklabels='True',
                       #  gridcolor = 'ffff',

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

                           list(step = "all"))),

                       rangeslider = list(type = "date")),
                     yaxis = list(title = locSWBplot_titles[which(input$SWBplot_y == locSWBplot_Ynames)]))

  })

  output$SWBplot <- plotly::renderPlotly({

    plot_SWB()

  })

########################################################################################################################
########################################################################################################################
  # ***** SWB Gridded

  spSWHC_dataInput <- reactive({
    req(input$SWHC_cdfInput)
    raster::brick(input$SWHC_cdfInput$datapath)
  })

  spRAIN_dataInput <- reactive({
    req(input$Rain_cdfInput)
    raster::brick(input$Rain_cdfInput$datapath)
  })

  spPET_dataInput <- reactive({
    req(input$PET_cdfInput)
    raster::brick(input$PET_cdfInput$datapath)
  })

  shinyFiles::shinyDirChoose(input, 'dirSWB', roots = c(home = '~'),
                             filetypes = c(" ", "nc", "xlsx", "tif"))

  global <- reactiveValues(datapath = getwd())
  dirSWB <- reactive(input$dirSWB)
  output$dirSWB <- renderText({global$datapath})

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dirSWB
               },
               handlerExpr = {
                 if (!"path" %in% names(dirSWB())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dirSWB()$path[-1]), collapse = .Platform$file.sep))
               })

  # render ui for slider ----------------------------------------------------

  output$spSWBtimeSlider <- renderUI({

    sliderInput("spSWBdateVec","SWB Time Slider: ",
                min = (lubridate::as_date(input$spSWB_DateStart)),
                max = (lubridate::as_date(input$spSWB_DateEnd)),
                value = (lubridate::as_date(input$spSWB_DateStart)),
                width = "100%",
                #      step = 3600,
                #     timezone = "+0000",
                animate = TRUE)
  })

  #######################################################################################################################

  SWBspNetCDF <-  eventReactive(input$SWBsp_runButton, {

    shinyWidgets::progressSweetAlert(
      session = session,      # getDefaultReactiveDomain(),
      id = "mySWBprogress",
      title = h3(paste0("SWB Estimation is in Progress ....."), style = "color: #FD1C03; font-style: bold; font-family: Brush Script MT;"),
      display_pct = TRUE,
      value = 0,
      size = 'xxs',
      striped = TRUE,
      width = '40%')

    # *********************************************************************************************************************


    DateStart.SWB = lubridate::as_date(input$spSWB_DateStart)
    DateEnd.SWB =  lubridate::as_date(input$spSWB_DateEnd)
    Res.SWB = input$SWBresInput
    outDir.SWB = global$datapath
    spSWB.outFile =  input$spSWBoutFile
    SWHCFile.SWB = spSWHC_dataInput()
    Rain.ncFile = spRAIN_dataInput()
    PET.ncFile = spPET_dataInput()

    dateVec.SWB <- seq.Date(from = lubridate::as_date(DateStart.SWB),
                            to = lubridate::as_date(DateEnd.SWB), by = "day")


    pet.ncData.r <- raster::raster(PET.ncFile[[1]])
    swbRtmplt <- raster::raster(raster::extent(pet.ncData.r), resolution = Res.SWB,
                                crs = raster::crs(pet.ncData.r), vals = 1)
    swbRsp <- as(swbRtmplt, "SpatialGridDataFrame")

    swbSWHCras <- raster::resample(SWHCFile.SWB, swbRtmplt, method = "bilinear")
    swbSWHCsp <- as(swbSWHCras, "SpatialGridDataFrame")

    rain.ncData <- Rain.ncFile
    rain.nms <- names(rain.ncData)
    rain.nms.date.vec <- lubridate::as_date(paste0(substr(rain.nms, 2, 5), "-", substr(rain.nms, 7, 8), "-",
                                                   substr(rain.nms, 10, 11)))
    startdate.SWBindex <- which(rain.nms.date.vec == DateStart.SWB)
    enddate.SWBindex <- which(rain.nms.date.vec == DateEnd.SWB)
    data.rain <- rain.ncData[[startdate.SWBindex:enddate.SWBindex]]

    pet.ncData <- PET.ncFile
    pet.nms <- names(pet.ncData)
    pet.nms.date.vec <- lubridate::as_date(paste0(substr(pet.nms, 2, 5), "-", substr(pet.nms, 7, 8), "-",
                                                  substr(pet.nms, 10, 11)))
    startdate.SWBindex <- which(pet.nms.date.vec == DateStart.SWB)
    enddate.SWBindex <- which(pet.nms.date.vec == DateEnd.SWB)
    data.pet <- pet.ncData[[startdate.SWBindex:enddate.SWBindex]]

    # ********************************************************************************************************************

    doParallel::registerDoParallel(cores = 4)

    rain.sp.list <- foreach(d = 1 : length(dateVec.SWB)) %dopar% {
      as(raster::resample(data.rain[[d]], swbRtmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.rain.sp <- snow::docall(snow::docall, rain.sp.list)

    pet.sp.list <- foreach(d = 1 : length(dateVec.SWB)) %dopar% {
      as(raster::resample(data.pet[[d]], swbRtmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.pet.sp <- snow::docall(snow::docall, pet.sp.list)



    DRAINsp <- swbRsp
    TRANsp <- swbRsp
    RUNOFFsp <- swbRsp
    AVAILsp <- swbRsp
    Rsp <- swbRsp

    DRAINsp@data <- data.frame(matrix(NA, nrow = nrow(swbRsp@data), ncol = length(dateVec.SWB)))
    TRANsp@data <- data.frame(matrix(NA, nrow = nrow(swbRsp@data), ncol = length(dateVec.SWB)))
    RUNOFFsp@data <- data.frame(matrix(NA, nrow = nrow(swbRsp@data), ncol = length(dateVec.SWB)))
    AVAILsp@data <- data.frame(matrix(NA, nrow = nrow(swbRsp@data), ncol = length(dateVec.SWB)))
    Rsp@data <- data.frame(matrix(NA, nrow = nrow(swbRsp@data), ncol = length(dateVec.SWB)))

    for (grd in 1:nrow(AVAILsp@data)) {

      data <- data.frame(Lat = sp::coordinates(AVAILsp)[,2][grd],
                         Lon = sp::coordinates(AVAILsp)[,1][grd],
                         Year = lubridate::year(dateVec.SWB),
                         Month = lubridate::month(dateVec.SWB),
                         Day = lubridate::day(dateVec.SWB),
                         Rain = as.numeric(data.rain.sp@data[grd, ]),
                         Eto = as.numeric(data.pet.sp@data[grd, ]))

      if (length(which(is.na(data$Rain))) > 1 || length(which(is.na(data$Eto))) > 1 || is.na(as.numeric(swbSWHCsp@data[grd,]))) {

        SWBgrd <- data
        SWBgrd$DRAIN <- NA
        SWBgrd$TRAN <- NA
        SWBgrd$RUNOFF <- NA
        SWBgrd$AVAIL <- NA
        SWBgrd$R <- NA

      } else {

        SWBgrd <- AquaBEHER::calcWatBal(data = data, soilWHC =  as.numeric(swbSWHCsp@data[grd,]))

      }

      DRAINsp@data[grd,] <- as.numeric(SWBgrd[,c("DRAIN")])
      TRANsp@data[grd,] <- as.numeric(SWBgrd[,c("TRAN")])
      RUNOFFsp@data[grd,] <- as.numeric(SWBgrd[,c("RUNOFF")])
      AVAILsp@data[grd,] <- as.numeric(SWBgrd[,c("AVAIL")])
      Rsp@data[grd,] <- as.numeric(SWBgrd[,c("R")])

      Sys.sleep(0.1)
      shinyWidgets::updateProgressBar(
        session = session,
        id = "mySWBprogress",
        value = grd,
        total = nrow(AVAILsp@data))
    }

    DRAINrasBRK <- terra::rast(raster::brick(DRAINsp))
    TRANrasBRK <- terra::rast(raster::brick(TRANsp))
    RUNOFFrasBRK <- terra::rast(raster::brick(RUNOFFsp))
    AVAILrasBRK <- terra::rast(raster::brick(AVAILsp))
    RrasBRK <- terra::rast(raster::brick(Rsp))

    terra::time(DRAINrasBRK) <- lubridate::as_date(dateVec.SWB)
    terra::time(TRANrasBRK) <- lubridate::as_date(dateVec.SWB)
    terra::time(RUNOFFrasBRK) <- lubridate::as_date(dateVec.SWB)
    terra::time(AVAILrasBRK) <- lubridate::as_date(dateVec.SWB)
    terra::time(RrasBRK) <- lubridate::as_date(dateVec.SWB)

    terra::writeCDF(DRAINrasBRK,
                    filename = paste0(outDir.SWB, "/", spSWB.outFile, "_DRAIN_", Res.SWB, "_",
                                      DateStart.SWB,"_T_", DateEnd.SWB, ".nc"),
                    varname = "DRAIN",
                    longname = paste0("amount of deep drainage in (mm)"),
                    unit = 'mm',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    terra::writeCDF(TRANrasBRK,
                    filename = paste0(outDir.SWB, "/", spSWB.outFile, "_TRAN_", Res.SWB, "_",
                                      DateStart.SWB,"_T_", DateEnd.SWB, ".nc"),
                    varname = "TRAN",
                    longname = paste0("amount of water lost by transpiration (after drainage) in (mm)"),
                    unit = 'mm',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    terra::writeCDF(RUNOFFrasBRK,
                    filename = paste0(outDir.SWB, "/", spSWB.outFile, "_RUNOFF_", Res.SWB, "_",
                                      DateStart.SWB,"_T_", DateEnd.SWB, ".nc"),
                    varname = "RUNOFF",
                    longname = paste0("surface runoff in (mm)"),
                    unit = 'mm',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    terra::writeCDF(AVAILrasBRK,
                    filename = paste0(outDir.SWB, "/", spSWB.outFile, "_Moisture_", Res.SWB, "_",
                                      DateStart.SWB,"_T_", DateEnd.SWB, ".nc"),
                    varname = "AVAIL",
                    longname = paste0("available soil moisture storage in (mm)"),
                    unit = 'mm',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    terra::writeCDF(RrasBRK,
                    filename = paste0(outDir.SWB, "/", spSWB.outFile, "_R_", Res.SWB, "_",
                                      DateStart.SWB,"_T_", DateEnd.SWB, ".nc"),
                    varname = "R",
                    longname = paste0("actual-to-potential evapotranspiration ratio"),
                    unit = 'index',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -9876543210,
                    overwrite = TRUE)

    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" SWB Calculation Completed !",
      type = "success"
    )

    SWBrasBRK.lst <- list(DRAINrasBRK,
                          TRANrasBRK,
                          RUNOFFrasBRK,
                          AVAILrasBRK,
                          RrasBRK)

    SWBrasBRK.lst

  })


  #######################################################################################################################

  #  output$spSWBmap <- leaflet::renderLeaflet(SWBbaseMap)

  output$spSWBmap = leaflet::renderLeaflet({

    leaflet::leaflet() %>%
      mapboxapi::addMapboxTiles(style_id = "satellite-streets-v12", username = "mapbox",
                                access_token = "pk.eyJ1Ijoicm9iZWx0YWtlbGUiLCJhIjoiY2xkb2o4NmRtMDEzcjNubHBkenMycnhiaSJ9.UkdfagqGIy7WjMGXtlT1mQ") %>%

      leaflet.multiopacity::addOpacityControls(group = "SWBlayers",
                                               collapsed = FALSE,
                                               position = "bottomleft",
                                               size = "m",
                                               title = "SWB Opacity Control:",
                                               renderOnLayerAdd = TRUE) %>%

      leaflet::addMiniMap(position = "bottomright",
                          width = 150,
                          height = 150) %>%
      leaflet::setView(lng = 38, lat = -14, zoom = 4)
  })



  observeEvent(input$SWBsp_runButton, {

    spSWB.mapLst <- SWBspNetCDF()

    spSWBlayr.index <- 5
    spSWBmap.index <- 1

    spSWB.mapDat <- spSWB.mapLst[[spSWBlayr.index]]



    spSWBmap.leaflet <- leaflet::projectRasterForLeaflet(spSWB.mapDat[[spSWBmap.index]], method = "bilinear")
    spSWBmap.colorPal <- leaflet::colorNumeric(spSWBcolPal[[spSWBlayr.index]], raster::values(spSWBmap.leaflet),
                                               na.color = "transparent")

    leaflet::leafletProxy("spSWBmap", session) %>%

      leaflet::addRasterImage(spSWBmap.leaflet, colors =  spSWBmap.colorPal, opacity = 0.8,
                              group = "SWBlayers", layerId = "SWBlayers")  %>%

      leaflet::addLegend(pal = spSWBmap.colorPal, values = raster::values(spSWBmap.leaflet),
                         title = spSWBlegTitle[spSWBlayr.index], position = "topright",
                         group = "SWBlayers", layerId = "SWBlayers")
  })

  spSWBmap.eventTrigger <- reactive({
    list(input$spSWB.mapView, input$spSWBdateVec)
  })

  output$spSWBmapTitle <- renderText({paste0("Soil Water-balance Parameters [ ",
                                             spSWBlegTitle[as.numeric(input$spSWB.mapView)], " ]: ",
                                             paste(input$spSWBdateVec))})

  observeEvent(ignoreInit = TRUE, spSWBmap.eventTrigger(), {


    DateStart.SWB = lubridate::as_date(input$spSWB_DateStart)
    DateEnd.SWB =  lubridate::as_date(input$spSWB_DateEnd)

    spSWB.dateVec <- seq.Date(from = lubridate::as_date(DateStart.SWB),
                              to = lubridate::as_date(DateEnd.SWB), by = "day")

    spSWBmap.index <- as.numeric(which(spSWB.dateVec == lubridate::as_date(input$spSWBdateVec)))

    spSWB.mapLst <- SWBspNetCDF()

    spSWBlayr.index <- as.numeric(input$spSWB.mapView)
    spSWB.mapDat <- spSWB.mapLst[[spSWBlayr.index]]

    spSWBmap.leaflet <- leaflet::projectRasterForLeaflet(spSWB.mapDat[[spSWBmap.index]], method = "bilinear")
    spSWBmap.colorPal <- leaflet::colorNumeric(spSWBcolPal[[spSWBlayr.index]], raster::values(spSWBmap.leaflet),
                                               na.color = "transparent")

    leaflet::leafletProxy("spSWBmap", session) %>% leaflet::clearControls() %>%

      leaflet::addRasterImage(spSWBmap.leaflet, colors =  spSWBmap.colorPal, opacity = 0.8,
                              group = "SWBlayers", layerId = "SWBlayers")  %>%

      leaflet::addLegend(pal = spSWBmap.colorPal, values = raster::values(spSWBmap.leaflet),
                         title = spSWBlegTitle[spSWBlayr.index], position = "topright",
                         group = "SWBlayers", layerId = "SWBlayers")   %>%

      leaflet.extras2::addEasyprint(options = leaflet.extras2::easyprintOptions(exportOnly = TRUE,
                                                                                hidden = TRUE,
                                                                                hideControlContainer = FALSE))
  })

  observeEvent(input$spSWBsave, {
    leaflet::leafletProxy("spSWBmap", session) %>%
      leaflet.extras2::easyprintMap(sizeModes = input$spSWBscene, filename = input$spSWBmapFileN)
  })


########################################################################################################################
########################################################################################################################
  # ***** seasCal

  seasCal_dataInput <- reactive({
    req(input$seasCal_xlsxInput)
    openxlsx::read.xlsx(input$seasCal_xlsxInput$datapath, sheet = 1)
  })

  seasCal_table <- eventReactive(input$seasCal_runButton, {

    seasCaldat <- seasCal_dataInput()
    seasCaldata.lst <- AquaBEHER::calcSeasCal(data = seasCaldat,
                                              onsetWind.start = as.Date(input$onsetWindstart),
                                              onsetWind.end = as.Date(input$onsetWindend),
                                              cessaWind.end = as.Date(input$cessaWindend),
                                              soilWHC = input$WHCinput)

    seasCaldata <- cbind.data.frame(seasCaldata.lst[[1]],
                                    seasCaldata.lst[[2]][,2:6],
                                    seasCaldata.lst[[3]][,2:4])
    seasCaldata

  })

  output$seasCaltable <- DT::renderDataTable(DT::datatable({

    seasCal_table()

  }))

  output$downloadseasCal <- downloadHandler(
    filename = function() {
      paste0("seasCal_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(seasCal_table(), file)
    }
  )

  plot_seasCal <- eventReactive(input$seasCal_plotButton, {

    data.seasCal <- seasCal_table()

    onsetDate <- lubridate::as_date(paste0(data.seasCal$onset.Year, "-", data.seasCal$onset.Month, "-", data.seasCal$onset.Day))
    onsetDate <- strftime(onsetDate, "%Y-%b-%d")

    cessnDate <- lubridate::as_date(paste0(data.seasCal$cessation.Year, "-", data.seasCal$cessation.Month,
                                           "-", data.seasCal$cessation.Day))
    cessnDate <- strftime(cessnDate, "%Y-%b-%d")
    durDate <- paste0(data.seasCal$Duration, " days")
    locWSClabs <- list(onsetDate, cessnDate, durDate)

    WSCplotTextPre <- c("Onset Date", "Cessation Date", "Duration")

    WSC.x.date <- data.seasCal$Year

    plotly::plot_ly( data = data.seasCal, # type = 'bar', color = I(input$WSCplot_color), showlegend = FALSE,
                     x = WSC.x.date,  alpha = 1, opacity = 0.8) %>%

      plotly::add_bars(y = data.seasCal[,input$WSCplot_y],
                       color = I(input$WSCplot_color),
                       textposition = "none",
                       textfont = list(family = "sans serif",
                                       color = "white",
                                       #   face="bold",
                                       size = I(30)),
                       text = paste0(WSCplotTextPre[which(input$WSCplot_y == WSCplot_Yvar)], ": ",
                                     locWSClabs[[which(input$WSCplot_y == WSCplot_Yvar)]]),
                       showlegend = FALSE) %>%


      plotly::config(displaylogo = FALSE,
                     toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                                 filename= paste0(input$WSCplot_y),
                                                 height= NULL,
                                                 width= NULL,
                                                 scale= 1 )) %>%

      plotly::layout(title = input$WSCplot_T, font = list(size = 12),
                     xaxis = list(
                       type = 'date',
                       tickformat = "%Y",
                       showticklabels='True',
                       #  gridcolor = 'ffff',

                       rangeselector = list(
                         buttons = list(
                           list(
                             count = 3,
                             label = "3 yr",
                             step = "year",
                             stepmode = "backward"),

                           list(
                             count = 5,
                             label = "5 yr",
                             step = "year",
                             stepmode = "backward"),

                           list(
                             count = 10,
                             label = "10 yr",
                             step = "year",
                             stepmode = "backward"),

                           list(
                             count = 1,
                             label = "YTD",
                             step = "year",
                             stepmode = "todate"),
                           list(step = "all"))),

                       rangeslider = list(type = "date")),
                     yaxis = list(title = WSCplot_Ynames[which(input$WSCplot_y == WSCplot_Yvar)]))

  })



  output$seasCalplot <- plotly::renderPlotly({

    plot_seasCal()

  })

########################################################################################################################
########################################################################################################################
  # ***** seasCal Gridded

  WSCspSWHC_dataInput <- reactive({
    req(input$WSCswhc.cdfInput)
    raster::brick(input$WSCswhc.cdfInput$datapath)
  })

  WSCspR_dataInput <- reactive({
    req(input$WSCr.cdfInput)
    raster::brick(input$WSCr.cdfInput$datapath)
  })

  WSCspSM_dataInput <- reactive({
    req(input$WSCsm.cdfInput)
    raster::brick(input$WSCsm.cdfInput$datapath)
  })

  shinyFiles::shinyDirChoose(input, 'dirWSC', roots = c(home = '~'),
                             filetypes = c(" ", "nc", "xlsx", "tif"))

  global <- reactiveValues(datapath = getwd())
  dirWSC <- reactive(input$dirWSC)
  output$dirWSC <- renderText({global$datapath})

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dirWSC
               },
               handlerExpr = {
                 if (!"path" %in% names(dirWSC())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dirWSC()$path[-1]), collapse = .Platform$file.sep))
               })

  # render ui for slider ----------------------------------------------------

  output$spWSCtimeSlider <- renderUI({

    spWSC.dateVec <- as.numeric(format(seq.Date(from = lubridate::as_date(input$spWSC_DateStart),
                                                to = lubridate::as_date(input$spWSC_DateEnd), by = "year"), "%Y"))

    sliderInput("spWSCdateVec","WSC Time Slider: ",
                min = spWSC.dateVec[1],
                max = spWSC.dateVec[length(spWSC.dateVec)],
                value = spWSC.dateVec[1],
                width = "100%",
                sep = NULL,
                step = 1,
                #     timezone = "+0000",
                animate = TRUE)
  })


  #######################################################################################################################

  WSCspNetCDF <- eventReactive(input$WSCsp_runButton, {

    shinyWidgets::progressSweetAlert(
      session = session,      # getDefaultReactiveDomain(),
      id = "myWSCprogress",
      title = h3(paste0("Seasonal Calendar Estimation is in Progress ....."),
                 style = "color: #FD1C03; font-style: bold; font-family: Brush Script MT;"),
      display_pct = TRUE,
      value = 0,
      size = '20px',
      striped = TRUE,
      width = '40%')

    # *********************************************************************************************************************

    DateStart.WSC = lubridate::as_date(input$spWSC_DateStart)
    DateEnd.WSC =  lubridate::as_date(input$spWSC_DateEnd)
    Res.WSC = input$WSCresInput
    outDir.WSC = global$datapath
    outFile.WSC = input$spWSCoutFilePrefix
    onsetWindstart = input$spWSConsetStart               #    "1996-09-01"
    onsetWindend = input$spWSConsetEnd                 #    "1997-01-31"
    cessaWindend = input$spWSCcessEnd                 #   "1997-06-30"

    #$  WSCrain.ncFile = WSCspRAIN_dataInput()
    WSCswhc.ncFile = WSCspSWHC_dataInput()
    WSCr.ncFile = WSCspR_dataInput()
    WSCsm.ncFile = WSCspSM_dataInput()


    dateVec.WSC <- seq.Date(from = lubridate::as_date(DateStart.WSC),
                            to = lubridate::as_date(DateEnd.WSC), by = "day")

    yearVec.WSC <- seq.Date(from = lubridate::as_date(DateStart.WSC),
                            to = lubridate::as_date(DateEnd.WSC), by = "year")

    WSCr.ncData.r <- raster::raster(WSCr.ncFile[[2]])
    wscRtmplt <- raster::raster(raster::extent(WSCr.ncData.r), resolution = Res.WSC,
                                crs = raster::crs(WSCr.ncData.r), vals = 1)
    wscRsp <- as(wscRtmplt, "SpatialGridDataFrame")

    wscSWHCras <- raster::resample(WSCswhc.ncFile, wscRtmplt, method = "bilinear")
    wscSWHCsp <- as(wscSWHCras, "SpatialGridDataFrame")


    WSCr.ncData <- WSCr.ncFile
    WSCr.nms <- names(WSCr.ncData)
    WSCr.nms.date.vec <- lubridate::as_date(paste0(substr(WSCr.nms, 2, 5), "-", substr(WSCr.nms, 7, 8), "-",
                                                   substr(WSCr.nms, 10, 11)))
    startdate.WSCindex <- which(WSCr.nms.date.vec == DateStart.WSC)
    enddate.WSCindex <- which(WSCr.nms.date.vec == DateEnd.WSC)
    data.WSCr <- WSCr.ncData[[startdate.WSCindex:enddate.WSCindex]]

    WSCsm.ncData <- WSCsm.ncFile
    WSCsm.nms <- names(WSCsm.ncData)
    WSCsm.nms.date.vec <- lubridate::as_date(paste0(substr(WSCsm.nms, 2, 5), "-", substr(WSCsm.nms, 7, 8), "-",
                                                    substr(WSCsm.nms, 10, 11)))
    startdate.WSCindex <- which(WSCsm.nms.date.vec == DateStart.WSC)
    enddate.WSCindex <- which(WSCsm.nms.date.vec == DateEnd.WSC)
    data.WSCsm <- WSCsm.ncData[[startdate.WSCindex:enddate.WSCindex]]

    # *********************************************************************************************************************

    doParallel::registerDoParallel(cores = 4)

    WSCr.sp.list <- foreach(d = 1 : length(dateVec.WSC)) %dopar% {
      as(raster::resample(data.WSCr[[d]], wscRtmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.WSCr.sp <- snow::docall(sp::cbind.Spatial, WSCr.sp.list)

    WSCsm.sp.list <- foreach(d = 1 : length(dateVec.WSC)) %dopar% {
      as(raster::resample(data.WSCsm[[d]], wscRtmplt, method = "bilinear"), "SpatialGridDataFrame")
    }

    data.WSCsm.sp <- snow::docall(sp::cbind.Spatial, WSCsm.sp.list)

    ONSETsp <- wscRsp
    yrONSETsp <- wscRsp
    CESSATIONsp <- wscRsp
    yrCESSATIONsp <- wscRsp
    DURATIONsp <- wscRsp

    ONSETsp@data <- data.frame(matrix(-999, nrow = nrow(wscRsp@data), ncol = length(yearVec.WSC)))
    yrONSETsp@data <- data.frame(matrix(-999, nrow = nrow(wscRsp@data), ncol = length(yearVec.WSC)))
    CESSATIONsp@data <- data.frame(matrix(-999, nrow = nrow(wscRsp@data), ncol = length(yearVec.WSC)))
    yrCESSATIONsp@data <- data.frame(matrix(-999, nrow = nrow(wscRsp@data), ncol = length(yearVec.WSC)))
    DURATIONsp@data <- data.frame(matrix(-999, nrow = nrow(wscRsp@data), ncol = length(yearVec.WSC)))

    for (grd in 1:nrow(ONSETsp@data)) {

      suppressWarnings({

        data <- data.frame(Lat = sp::coordinates(data.WSCr.sp)[,2][grd],
                           Lon = sp::coordinates(data.WSCr.sp)[,1][grd],
                           Year = lubridate::year(dateVec.WSC),
                           Month = lubridate::month(dateVec.WSC),
                           Day = lubridate::day(dateVec.WSC),
                           R = as.numeric(data.WSCr.sp@data[grd, ]),
                           AVAIL = as.numeric(data.WSCsm.sp@data[grd, ]))


        if (length(which(is.na(data$R))) < 1 && length(which(is.na(data$AVAIL))) < 1  && !is.na(as.numeric(wscSWHCsp@data[grd,]))) {

          WSCgrd <- AquaBEHER::calcSeasCal(data = data,
                                           onsetWind.start = onsetWindstart,
                                           onsetWind.end = onsetWindend,
                                           cessaWind.end = cessaWindend,
                                           soilWHC = as.numeric(wscSWHCsp@data[grd,]))

        } else {

          WSCgrd <- list(data.frame(Year = sort(unique(data$Year)),
                                    onset.Year = NA,
                                    onset.Month = NA,
                                    onset.Day = NA,
                                    onset.JD = NA,
                                    onset.Value = NA),

                         data.frame(Year = sort(unique(data$Year)),
                                    cessation.Year = NA,
                                    cessation.Month = NA,
                                    cessation.Day = NA,
                                    cessation.JD = NA,
                                    cessation.Value = NA),

                         data.frame(Year = sort(unique(data$Year)),
                                    onset.YYYYDOY = NA,
                                    cessation.YYYYDOY = NA,
                                    Duration= NA))
        }


        oVal1 <- as.numeric(WSCgrd[[1]][,c("onset.Value")])
        oVal1L <- length(oVal1)
        oVal2 <- as.numeric(paste0(WSCgrd[[1]][,c("onset.Year")], WSCgrd[[1]][,c("onset.Month")], WSCgrd[[1]][,c("onset.Day")]))
        oVal2L <- length(oVal2)

        cVal1 <- as.numeric(WSCgrd[[2]][,c("cessation.Value")])
        cVal1L <- length(cVal1)
        cVal2 <- as.numeric(paste0(WSCgrd[[2]][,c("cessation.Year")], WSCgrd[[2]][,c("cessation.Month")], WSCgrd[[2]][,c("cessation.Day")]))
        cVal2L <- length(cVal2)

        dVal1 <- as.numeric(WSCgrd[[3]][,c("Duration")])
        dval1L <- length(dVal1)

        ONSETsp@data[grd, 1: oVal1L] <- oVal1
        yrONSETsp@data[grd, 1: oVal2L] <- oVal2

        CESSATIONsp@data[grd, 1: cVal1L] <- cVal1
        yrCESSATIONsp@data[grd, 1: cVal2L] <- cVal2

        DURATIONsp@data[grd, 1:dval1L] <- dVal1

      })

      Sys.sleep(0.1)
      shinyWidgets::updateProgressBar(
        session = session,
        id = "myWSCprogress",
        value = grd,
        total = nrow(ONSETsp@data))

    }

    ONSETrasBRK <- terra::rast(raster::brick(ONSETsp))
    yrONSETrasBRK <- terra::rast(raster::brick(yrONSETsp))

    terra::time(ONSETrasBRK) <- lubridate::as_date(yearVec.WSC[1:(length(yearVec.WSC))])
    terra::time(yrONSETrasBRK) <- lubridate::as_date(yearVec.WSC[1:(length(yearVec.WSC))])

    onsetSDS <- terra::sds(yrONSETrasBRK, ONSETrasBRK)
    names(onsetSDS) <- c("onset.Date", "onset.Value")
    terra::longnames(onsetSDS) <- c("Onset Date", paste0("Onset value: No. of days since start of onset window: ",
                                                         strftime(onsetWindstart, "%b-%d")))
    terra::units(onsetSDS) <- c("YYYY-MM-DD", paste0("days since ", strftime(onsetWindstart, "%b-%d")))

    terra::writeCDF(x = onsetSDS,
                    filename = paste0(outDir.WSC, "/", outFile.WSC, "_ONSET_", Res.WSC, "deg_",
                                      substr(yearVec.WSC[1],1,4),"T", substr(yearVec.WSC[length(yearVec.WSC)], 1, 4), ".nc"),
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -999,
                    overwrite = TRUE)

    CESSATIONrasBRK <- terra::rast(raster::brick(CESSATIONsp))
    yrCESSATIONrasBRK <- terra::rast(raster::brick(yrCESSATIONsp))

    terra::time(CESSATIONrasBRK) <- lubridate::as_date(yearVec.WSC[1:(length(yearVec.WSC))])
    terra::time(yrCESSATIONrasBRK) <- lubridate::as_date(yearVec.WSC[1:(length(yearVec.WSC))])

    CESSATIONSDS <- terra::sds(yrCESSATIONrasBRK, CESSATIONrasBRK)
    names(CESSATIONSDS) <- c("cessation.Date", "cessation.Value")
    terra::longnames(CESSATIONSDS) <- c("Cessation Date", paste0("Cessation value: No. of days since start of onset window: ",
                                                                 strftime(onsetWindstart, "%b-%d")))
    terra::units(CESSATIONSDS) <- c("YYYY-MM-DD", paste0("days since ", strftime(onsetWindstart, "%b-%d")))

    terra::writeCDF(x = CESSATIONSDS,
                    filename = paste0(outDir.WSC, "/", outFile.WSC, "_CESSATION_", Res.WSC, "deg_",
                                      substr(yearVec.WSC[1],1,4),"T", substr(yearVec.WSC[length(yearVec.WSC)], 1, 4), ".nc"),
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -999,
                    overwrite = TRUE)

    DURATIONrasBRK <- terra::rast(raster::brick(DURATIONsp))
    terra::time(DURATIONrasBRK) <- lubridate::as_date(yearVec.WSC[1:(length(yearVec.WSC))])

    terra::writeCDF(DURATIONrasBRK,
                    filename = paste0(outDir.WSC, "/", outFile.WSC, "_DURATION_", Res.WSC, "deg_",
                                      substr(yearVec.WSC[1],1,4),"T", substr(yearVec.WSC[length(yearVec.WSC)], 1, 4), ".nc"),
                    varname = "duration",
                    longname = paste0("total duration of the wet-season in days"),
                    unit = 'days',
                    zname = 'time',
                    prec = "double",
                    compression = 9,
                    missval = -999,
                    overwrite = TRUE)

    DURATIONSDS <- terra::sds(DURATIONrasBRK,  DURATIONrasBRK)


    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(
      session = session,
      title =" WSC Calculation Completed !",
      type = "success")


    WSCrasBRK.lst <- list(onsetSDS,  CESSATIONSDS,  DURATIONSDS)

    WSCrasBRK.lst

  }
  )


  ########################################################################################################################

  output$spSeasCALmap = leaflet::renderLeaflet({

    leaflet::leaflet() %>%
      mapboxapi::addMapboxTiles(style_id = "satellite-streets-v12", username = "mapbox",
                                access_token = "pk.eyJ1Ijoicm9iZWx0YWtlbGUiLCJhIjoiY2xkb2o4NmRtMDEzcjNubHBkenMycnhiaSJ9.UkdfagqGIy7WjMGXtlT1mQ") %>%

      leaflet.multiopacity::addOpacityControls(group = "WSClayers",
                                               collapsed = FALSE,
                                               position = "bottomleft",
                                               size = "m",
                                               title = "WSC Opacity Control:",
                                               renderOnLayerAdd = TRUE) %>%

      leaflet::addMiniMap(position = "bottomright",
                          width = 150,
                          height = 150) %>%

      leaflet::setView(lng = 38, lat = -14, zoom = 4)
  })


  observeEvent(input$WSCsp_runButton, {

    spWSC.mapLst <- WSCspNetCDF()

    spWSClayr.index <- 1
    spWSCmap.index <- 1

    spWSC.mapDat <- spWSC.mapLst[[spWSClayr.index]][2]
    spWSC.mapDat[is.na(spWSC.mapDat)] <- -999

    spWSCmap.leaflet <- leaflet::projectRasterForLeaflet(spWSC.mapDat[[spWSCmap.index]], method = "bilinear")
    spWSCmap.colorPal <- leaflet::colorNumeric(spWSCcolPal[[spWSClayr.index]], raster::values(spWSCmap.leaflet),
                                               na.color = "transparent")

    leaflet::leafletProxy("spSeasCALmap", session) %>%

      leaflet::addRasterImage(spWSCmap.leaflet, colors =  spWSCmap.colorPal, opacity = 0.8,
                              group = "WSClayers", layerId = "WSClayers")  %>%

      leaflet::addLegend(pal = spWSCmap.colorPal, values = raster::values(spWSCmap.leaflet),
                         title = spWSClegTitle[spWSClayr.index], position = "topright",
                         group = "WSClayers", layerId = "WSClayers")
  }
  )

  spWSCmap.eventTrigger <- reactive({
    list(input$spWSC.mapView, input$spWSCdateVec)
  })

  output$spWSCmapTitle <- renderText({paste0("Seasonal Calendar [ ",
                                             spWSClegTitle[as.numeric(input$spWSC.mapView)], " ]: ",
                                             paste(input$spWSCdateVec))})


  observeEvent(ignoreInit = TRUE, spWSCmap.eventTrigger(), {

    spWSC.dateVec <- as.numeric(format(seq.Date(from = lubridate::as_date(input$spWSC_DateStart),
                                                to = lubridate::as_date(input$spWSC_DateEnd), by = "year"), "%Y"))

    spWSCmap.index <- as.numeric(which(spWSC.dateVec == as.numeric(input$spWSCdateVec)))

    spWSC.mapLst <-  WSCspNetCDF()

    spWSClayr.index <- as.numeric(input$spWSC.mapView)
    spWSC.mapDat <- spWSC.mapLst[[spWSClayr.index]][2]
    spWSC.mapDat[is.na(spWSC.mapDat)] <- -999

    spWSCmap.leaflet <- leaflet::projectRasterForLeaflet(spWSC.mapDat[[spWSCmap.index]], method = "bilinear")
    spWSCmap.colorPal <- leaflet::colorNumeric(rev(pals::cubicl(100)), raster::values(spWSCmap.leaflet),
                                               na.color = "transparent")

    leaflet::leafletProxy("spSeasCALmap", session) %>% leaflet::clearControls() %>%

      leaflet::addRasterImage(spWSCmap.leaflet, colors =  spWSCmap.colorPal, opacity = 0.8,
                              group = "WSClayers", layerId = "WSClayers")  %>%

      leaflet::addLegend(pal = spWSCmap.colorPal, values = raster::values(spWSCmap.leaflet),
                         title = spWSClegTitle[spWSClayr.index], position = "topright",
                         group = "WSClayers", layerId = "WSClayers") %>%

      leaflet.extras2::addEasyprint(options = leaflet.extras2::easyprintOptions(exportOnly = TRUE,
                                                                                hidden = TRUE,
                                                                                hideControlContainer = FALSE))
  })

  # spWSCmapSaver(id = "spWSCsave")

  observeEvent(input$spWSCsave, {
    leaflet::leafletProxy("spSeasCALmap", session) %>%
      leaflet.extras2::easyprintMap(sizeModes = input$spWSCscene, filename = input$spWSCmapFileN)
  })

############################################################################################################
############################################################################################################
 # Automatically stop the shiny app when closing the browser tab

  session$onSessionEnded(stopApp)

}

############################################################################################################
############################################################################################################
############################################################################################################
