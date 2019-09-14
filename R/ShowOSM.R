ShowOSM <- function(boundingBox,
                    trackList = NULL,
                    graticuleInterval = 0.1
){
  
  if (is.data.frame(trackList)){
    trackList <- list(list(table = trackList))
  }
  
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
  if (!is.null(trackList)){
    for (trackId in 1:length(trackList)){
      mapObject <- mapObject %>%
      addPolylines(map = .,
                   lat = trackList[[trackId]]$table$lat, 
                   lng = trackList[[trackId]]$table$lon,
                   weight = 3, opacity = 1.0,
                   color = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', 
                             '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
                             '#bcf60c', '#fabebe', '#008080', '#e6beff', 
                             '#9a6324', '#fffac8', '#800000', '#aaffc3', 
                             '#808000', '#ffd8b1', '#000075', '#808080'
                   )[trackId],
                   highlightOptions = highlightOptions(bringToFront = T, opacity = 1, weight = 5, sendToBack = FALSE, color = 'white')
      )
    }
  }
  
  # Markers
  if (!is.null(trackList)){
    for (trackId in 1:length(trackList)){
      markerLabels <- as.list(paste0(sprintf('%03d', trackId), '-', 1:nrow(trackList[[trackId]]$table)))
      mapObject <- mapObject %>% 
        addCircleMarkers(lat = trackList[[trackId]]$table$lat, 
                         lng = trackList[[trackId]]$table$lon,
                         layerId = markerLabels,
                         label = markerLabels,
                         stroke = FALSE,fillOpacity = 0.8,
                         clusterOptions = markerClusterOptions()
        )
    }
  }

  mapObject
}