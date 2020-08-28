AssesTrackOnMap <- function(trackList,
                            graticuleInterval = 0.1,
                            go.showCumulative = T
){
  
  if (is.data.frame(trackList)){
    trackList <- list(list(table = trackList))
  }
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    tags$style(type = "text/css", "#myMap {height: calc(100vh - 80px) !important;}"),
    actionButton('doneButton', 'Done'),
    p()
  )
  
  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({ShowOSM(boundingBox = boundingBox, 
                                           graticuleInterval = graticuleInterval, 
                                           trackList = trackList
    )})
    
    ## Observe "Done" button events
    observeEvent(input$doneButton, {
      stopApp()
    })
    
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
      
      if (go.showCumulative){
        pointText <- sprintf('%.1f km %.0f m', pointTable$cumDist/1e3, pointTable$cumGain_pos) 
      } else {
        pointText <- as.character(nrow(pointTable))
      }

      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1, label = pointText,
                   labelOptions = labelOptions(noHide = T, direction = 'top')
        )
      
      print('---------------------------------------')
      print(pointTable)
      
      
    })

  }
  
  shinyApp(ui, server)
}