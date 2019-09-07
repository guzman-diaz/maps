SelectPoints <- function(boundingBox, graticuleInterval = 0.1, trackPoints = trackTable){
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    p()
  )
  
  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval, trackPoints = trackTable
    )})
    
    observeEvent(input$myMap_click, {
      clickedPoint <- input$myMap_click
      pointCoords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat)
      print(pointCoords)
      
      pointTable <<- rbind(pointTable, pointCoords)
      
      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 10, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })
  }
  
  shinyApp(ui, server)
  
}