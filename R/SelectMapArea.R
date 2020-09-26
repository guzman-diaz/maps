SelectMapArea <- function(boundingBox = NULL,
                          graticuleInterval = 0.1,
                          environment = .GlobalEnv
){
  
  pacman::p_load(shiny)
  
  # If no bbox provided, select Gijón
  if (is.null(boundingBox)){
    ## Lon-lat coordinates as data frame
    boundingBox <- data.frame(x = c(-5.75, -5.55), y = c(43.45, 43.60)) # x = lon; y = lat
    
    ## Transform data frame to SpatialPoints class
    sp::coordinates(boundingBox) <- names(boundingBox)
    
    ## Assign lon-lat CRS: epsg:4326
    sp::proj4string(boundingBox) <- sp::CRS('+init=epsg:4326') # lon-lat
    
    ## Define bounding box 
    boundingBox <- boundingBox %>% 
      raster::extent()
  }
  
  # Window properties
  ui <- fluidPage(
    leafletOutput('myMap'),
    tags$style(type = "text/css", "#myMap {height: calc(100vh - 80px) !important;}"),
    actionButton('doneButton', 'Done'),
    p()
  )
  
  # Shiny server
  server <- function(input, output, session) {
    
    ## Launch map
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval
    )})
    
    ## Observe "Done" button events
    observeEvent(input$doneButton, {
      p1 <- c(lon = input$myMap_bounds$west, lat = input$myMap_bounds$south)
      p2 <- c(lon = input$myMap_bounds$east, lat = input$myMap_bounds$north)

      boundingBox <- list(p1 = as.list(p1), p2 = as.list(p2))
      
      boundingBox$p1 <- c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = T))
      boundingBox$p2 <- c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = T))

      assign('boundingBox', boundingBox, envir = environment)
      stopApp()
    })
    
  }
  
  app <- shinyApp(ui = ui, server = server)
  
  runApp(app)

}