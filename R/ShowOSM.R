ShowOSM <- function(boundingBox,
                    trackTable = NULL,
                    markerTable = NULL,
                    trackName = NULL,
                    graticuleInterval = 0.1
){
  
  pacman::p_load(leaflet)

  mapObject <- leaflet() %>%
    addTiles(
      paste0(
        'https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b',
        Sys.getenv("OCM_API")
      )
    )%>% 
    addProviderTiles('Thunderforest.OpenCycleMap', group = 'Topographical') %>%
    addProviderTiles('OpenStreetMap.Mapnik', group = 'Road map') %>%
    addProviderTiles('Esri.WorldImagery', group = 'Satellite') %>%
    addRectangles(
      lng1 = boundingBox$p1$lon, lat1 = boundingBox$p1$lat,
      lng2 = boundingBox$p2$lon, lat2 = boundingBox$p2$lat,
      fillColor = 'transparent', weight = '2',
    ) %>%
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
    )
  
  # Show track
  if (!is.null(trackTable)) {
    mapObject <- mapObject %>% 
      addPolylines(map = ., 
                   lng = trackTable$lon, lat = trackTable$lat, 
                   weight = 2, opacity = 0.8,
                   highlightOptions = highlightOptions(bringToFront = T, opacity = 1, weight = 5, sendToBack = FALSE, color = 'white')
      )
  }
  
  # Markers
  if (!is.null(markerTable)){
    mapObject <- mapObject %>% 
      addMarkers(lat = markerTable$lat, 
                 lng = markerTable$lon,
                 layerId = paste(if (!is.null(trackName)) {trackName}, 1:nrow(markerTable)),
                 popup = paste(if (!is.null(trackName)) {trackName}, 1:nrow(markerTable))
      )
  }

  mapObject
}