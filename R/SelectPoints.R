SelectPoints <- function(boundingBox, graticuleInterval = 0.1, trackPoints = trackTable){
  
  
  ui <- fluidPage(
    leafletOutput('myMap'),
    p()
  )

  server <- function(input, output, session) {
    
    output$myMap <- renderLeaflet({
      leaflet() %>%
        addTiles(
          paste0(
            'https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b',
            Sys.getenv("OCM_API")
          )
        )%>% 
        addProviderTiles('Thunderforest.OpenCycleMap', group = 'Topographical') %>%
        addProviderTiles('OpenStreetMap.Mapnik', group = 'Road map') %>%
        addProviderTiles('Esri.WorldImagery', group = 'Satellite') %>%
        fitBounds(
          lng1 = boundingBox$p1$lon, lat1 = boundingBox$p1$lat,
          lng2 = boundingBox$p2$lon, lat2 = boundingBox$p2$lat,
        ) %>% 
        addSimpleGraticule(interval = graticuleInterval,
                           group = 'Graticule'
        ) %>% 
        addLayersControl(position = 'bottomright',
                         baseGroups = c('Topographical', 'Road map', 'Satellite'),
                         overlayGroups = c('Graticule'),
                         options = layersControlOptions(collapsed = FALSE)
        ) %>%
        { if (!is.null(trackPoints)) 
          addPolylines(map = ., lng = trackPoints$lon, lat = trackPoints$lat, weight = 2, opacity = 0.8) 
          else . 
        }
    })
    
    observeEvent(input$myMap_click, {
      click <- input$myMap_click
      clat <- click$lat
      clng <- click$lng
      
      print(c(clng, clat))
      print(click)
      
      # selectedPoints <<-c(list(c(lon = clng, lat = clat)), selectedPoints)
      selectedPoints <<- rbind(selectedPoints, c(lon = clng, lat = clat))
      
      leafletProxy('myMap') %>%
        addCircles(lng=clng, lat=clat, group='circles',
                   weight=1, radius=10, color='black', fillColor='green',
                   fillOpacity=0.2, opacity=1)
    })
  }
  
  shinyApp(ui, server)
  
}