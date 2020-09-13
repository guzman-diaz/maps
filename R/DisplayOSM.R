DisplayOSM <- function(mapObject_sp = NULL,
                         track_lst = NULL,
                         graticuleInterval = 0.1
){
  
  pacman::p_load(leaflet)
  
  mapObject_leaflet <- leaflet() %>%
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
      lng1 = raster::extent(mapObject_sp)@xmin, lat1 = raster::extent(mapObject_sp)@ymin,
      lng2 = raster::extent(mapObject_sp)@xmax, lat2 = raster::extent(mapObject_sp)@ymax,
      fillColor = 'transparent', weight = '2',
    ) %>%
    fitBounds(
      lng1 = raster::extent(mapObject_sp)@xmin, lat1 = raster::extent(mapObject_sp)@ymin,
      lng2 = raster::extent(mapObject_sp)@xmax, lat2 = raster::extent(mapObject_sp)@ymax,
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
  if (!is.null(track_lst)){
    for (track_id in 1:length(track_lst)){
      mapObject_leaflet <- mapObject_leaflet %>%
      addPolylines(map = .,
                   lng = track_lst[[track_id]]@coords[, 1],
                   lat = track_lst[[track_id]]@coords[, 2], 
                   weight = 3, opacity = 1.0,
                   color = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', 
                             '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
                             '#bcf60c', '#fabebe', '#008080', '#e6beff', 
                             '#9a6324', '#fffac8', '#800000', '#aaffc3', 
                             '#808000', '#ffd8b1', '#000075', '#808080'
                   )[track_id],
                   highlightOptions = highlightOptions(bringToFront = T, opacity = 1, weight = 5, sendToBack = FALSE, color = 'white')
      )
    }
  }
  
  # Markers
  if (!is.null(track_lst)){
    for (track_id in 1:length(track_lst)){
      markerLabels <- as.list(paste0(sprintf('%03d', track_id), '-', 1:nrow(track_lst[[track_id]]@coords)))
      mapObject_leaflet <- mapObject_leaflet %>% 
        addCircleMarkers(lng = track_lst[[track_id]]@coords[, 1],
                         lat = track_lst[[track_id]]@coords[, 2], 
                         layerId = markerLabels,
                         label = markerLabels,
                         stroke = FALSE,fillOpacity = 0.8,
                         clusterOptions = markerClusterOptions()
        )
    }
  }

  print(mapObject_leaflet)
}