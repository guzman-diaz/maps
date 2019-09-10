SelectPoints <- function(boundingBox, 
                         graticuleInterval = 0.1, 
                         trackList = NULL
){

  if (!exists('pointTable', envir = .GlobalEnv)){
    pointTable <- data.frame(matrix(nrow = 0, ncol = 3)) %>% setNames(c('lon', 'lat', 'id'))
    assign('pointTable', pointTable, envir = .GlobalEnv)
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

      pointTable <<- rbind(pointTable, as.data.frame(t(pointCoords)))
      assign('pointTable', pointTable, envir =  .GlobalEnv)
      
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

      pointTable <<- rbind(pointTable, as.data.frame(t(pointCoords))) %>% 
        dplyr::distinct(lon, lat, .keep_all = T)
      assign('pointTable', pointTable, envir =  .GlobalEnv)
      print(sprintf('Point %i added', nrow(pointTable)))
      
      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1, label = as.character(nrow(pointTable)),
                   labelOptions = labelOptions(noHide = T, direction = 'top')
        )
    })

  }
  
  shinyApp(ui, server)
}