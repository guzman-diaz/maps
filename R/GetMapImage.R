GetMapImage <- function(boundingBox,
                        mapType = 'esri-topo',
                        mapDim = NULL,
                        imageFileName = here::here('figs', 'overlay.png')
){
  
  pacman::p_load(OpenStreetMap)
  
  # Map types
  if (is.numeric(mapType)){
    mapType <- c('osm', 'bing', 'stamen-toner', 
                 'maptoolkit-topo',
                 'waze', 'mapquest', 'mapquest-aerial',
                 'stamen-terrain',
                 'stamen-watercolor', 'osm-german', 'osm-wanderreitkarte',
                 'mapbox', 'esri', 'esri-topo',
                 'nps', 'apple-iphoto', 'skobbler',
                 'opencyclemap', 'osm-transport',
                 'osm-public-transport', 'osm-bbike', 'osm-bbike-german'
    )[mapType]
    
    print(sprintf('OSM map name: %s', mapType))
  }
  
  # Download map
  mapObject <-openmap(upperLeft = c(lat = boundingBox$p2$lat, lon = boundingBox$p1$lon),
                      lowerRight = c(lat = boundingBox$p1$lat, lon = boundingBox$p2$lon), 
                      zoom = NULL, 
                      type = mapType,
                      mergeTiles = TRUE
  )
  
  # Define image width
  imageHeight <- mapDim[2]
  imageWidth <- mapDim[1]
  
  # fetch overlay image
  png(filename = imageFileName, width = imageWidth, height = imageHeight)
  plot(mapObject)
  dev.off()
  
  return(png::readPNG(here::here('figs', 'overlay.png')))
  
}

