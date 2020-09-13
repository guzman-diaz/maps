ReprojectLonLatUTM <- function(inputObject){
  
  # Define CRS projections
  proj4string_lonlat <- sp::CRS('+init=epsg:4326')
  proj4string_utm30 <- sp::CRS('+init=epsg:32630')
  
  
  # Transform
  if (raster::compareCRS(inputObject, proj4string_lonlat)){
    ## If data is lonlat
    if (is(inputObject, 'SpatialPoints')){
      ## If it is a SP object (track)
      outputObject <- sp::spTransform(inputObject, proj4string_utm30)
    }
  } else {
    ## If data is UTM30
    if (is(inputObject, 'SpatialPoints')){
      ## If it is a SP object (track)
      outputObject <- sp::spTransform(inputObject, proj4string_lonlat)
    }
  }
  
  
  # Output
  return(outputObject)
  
}
