SelectMapArea <- function(boundingBox = NULL,
                          graticuleInterval = 0.1
){
  
  if (is.null(boundingBox)){
    boundingBox <- DefineBoundingBox(p1 = c(lon = -5.75, lat = 43.45), p2 = c(lon = -5.55, lat = 43.6))
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
      boundingBox <- DefineBoundingBox(
        p1 = c(lon = input$myMap_bounds$west,
               lat = input$myMap_bounds$south
        ),
        p2 = c(lon = input$myMap_bounds$east,
               lat = input$myMap_bounds$north
        )
      )
      assign('boundingBox', boundingBox, envir = .GlobalEnv)
      stopApp()
    })
    
  }
  
  app <- shinyApp(ui = ui, server = server)
  
  runApp(app)

}