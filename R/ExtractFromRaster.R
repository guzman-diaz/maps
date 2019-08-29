ExtractFromRaster <- function(rasterObject){
  
  # Convert raster object to matrix
  mapMatrix <- matrix(raster::extract(rasterObject, 
                                      extent(rasterObject), 
                                      buffer = 1000
  ),
  nrow = ncol(rasterObject), 
  ncol = nrow(rasterObject)
  )
  
  # Bounding box
  ## xy
  boundingBox <- list(
    p1 = as.list(c(x = extent(rasterObject)@xmin, y = extent(rasterObject)@ymin)),
    p2 = as.list(c(x = extent(rasterObject)@xmax, y = extent(rasterObject)@ymax))
  )
  
  ## lonlat
  boundingBox$p1 <-  c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = F))
  boundingBox$p2 <-  c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = F))
  
  # Output
  return(list(elevation = mapMatrix,
              extent = extent(rasterObject),
              boundingBox = boundingBox
  ))
  
}