ShowOSM <- function(boundingBox,
                    graticuleInterval = 0.1
){
  
  pacman::p_load(leaflet)
  
  leaflet() %>%
    addTiles() %>% 
    addRectangles(
      lng1 = boundingBox$p1$lon, lat1 = boundingBox$p1$lat,
      lng2 = boundingBox$p2$lon, lat2 = boundingBox$p2$lat,
      fillColor = 'transparent'
    ) %>%
    fitBounds(
      lng1 = boundingBox$p1$lon, lat1 = boundingBox$p1$lat,
      lng2 = boundingBox$p2$lon, lat2 = boundingBox$p2$lat,
    ) %>% 
    addSimpleGraticule(interval = graticuleInterval,
                       group = 'Graticule'
    ) %>% 
    addLayersControl(overlayGroups = c('Graticule'),
                     options = layersControlOptions(collapsed = FALSE)
    )
  
}