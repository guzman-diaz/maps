SelectMapArea <- function(boundingBox = NULL,
                          graticuleInterval = 0.1,
                          environment = .GlobalEnv
){
  
  if (is.null(boundingBox)){
    boundingBox <- list(p1 = as.list(c(lon = -5.75, lat = 43.45)), 
                        p2 = as.list(c(lon = -5.55, lat = 43.60))
    )
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