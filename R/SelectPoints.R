SelectPoints <- function(boundingBox, 
                         graticuleInterval = 0.1, 
                         trackTable = NULL,
                         markerTable = NULL
){
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    p()
  )
  
  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval, 
                                           trackTable = trackTable,
                                           markerTable = markerTable
    )})
    
    observeEvent(input$myMap_click, {
      clickedPoint <- input$myMap_click
      pointCoords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat)
      print(pointCoords)
      
      pointTable <<- rbind(pointTable, pointCoords)
      
      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })
    
    observeEvent(input$myMap_marker_click, {
      clickedPoint <- input$myMap_marker_click
      pointCoords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat)
      print(pointCoords)
      
      pointTable <<- rbind(pointTable, pointCoords)
      
      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })
    
      }
  
  shinyApp(ui, server)
  
}