AssesTrackOnMap <- function(track_lst,
                            graticuleInterval = 0.01,
                            go.showCumulative = T,
                            boundingBox = boundingBox,
                            rasterObject
){
  
  if (is.data.frame(track_lst)){
    track_lst <- list(list(table = track_lst))
  }
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    tags$style(type = "text/css", "#myMap {height: calc(100vh - 80px) !important;}"),
    actionButton('ending', 'Done'),
    p()
  )
  
  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({DisplayOSM(boundingBox, 
                                              track_lst = track_lst,
                                              graticuleInterval = 0.1
    )})
    
    ## Observe "Done" button events
    observeEvent(input$doneButton, {
      stopApp()
    })
    
    ## Observe clicks on markers of tracks
    observeEvent(input$ending, {
      stopApp()
    })
    
    observeEvent(input$myMap_marker_click, {
      clickedPoint <- input$myMap_marker_click
      points_coords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = clickedPoint$id)
      
      ProcessSelectedPoints(track_lst = track_lst, points_coords = points_coords, rasterObject = rasterObject)
      
      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })
    
    ## Observe clicks on any point of the map
    observeEvent(input$myMap_click, {
      clickedPoint <- input$myMap_click
      points_coords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = 'map')
      
      pointTable <- ProcessSelectedPoints(track_lst = track_lst, points_coords = points_coords, rasterObject = rasterObject)
      
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