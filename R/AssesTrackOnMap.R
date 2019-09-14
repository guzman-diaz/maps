AssesTrackOnMap <- function(trackList,
                            graticuleInterval = 0.01
){
  
  if (is.data.frame(trackList)){
    trackList <- list(list(table = trackList))
  }
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    tags$style(type = "text/css", "#myMap {height: calc(100vh - 80px) !important;}"),
    p()
  )
  
  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval, 
                                           trackList = trackList
    )})
    
    ## Observe clicks on markers
    observeEvent(input$myMap_marker_click, {
      clickedPoint <- input$myMap_marker_click
      pointCoords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = clickedPoint$id)

      ProcessSelectedPoints(trackList = trackList, pointCoords = pointCoords)

      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })
    
    ## Observe clicks on any point of the map
    observeEvent(input$myMap_click, {
      clickedPoint <- input$myMap_click
      pointCoords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = 'map')

      pointTable <- ProcessSelectedPoints(trackList = trackList, pointCoords = pointCoords)

      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1, label = as.character(nrow(pointTable)),
                   labelOptions = labelOptions(noHide = T, direction = 'top')
        )
      
      print('---------------------------------------')
      print(pointTable)
    })

  }
  
  shinyApp(ui, server)
}