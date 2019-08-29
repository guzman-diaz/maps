CropRaster <- function(rasterObject, boundingBox, zoomLevel = 1){
  
  # Check in xy coordinates are present
  if (is.null(boundingBox$p1$x)){
    boundingBox$p1 <-  c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = T))
  }
  
  if (is.null(boundingBox$p2$x)){
    boundingBox$p2 <-  c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = T))
  }
  
  # Apply zoom
  boundingBoxDiagonal <- 
  
  boundingBox <- with(boundingBox,
                      p1$x <- p1$x
  )
  
  # Define polygon
  cropPolygon <- with(boundingBox,
                      as(raster::extent(list(x = c(p1$x, p2$x), y = c(p1$y, p2$y))), 
                      'SpatialPolygons'
                      )
  )
  
  # Crop
  rasterObject <- raster::crop(rasterObject, cropPolygon)
  
}