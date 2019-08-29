CropRaster <- function(rasterObject, boundingBox, zoomLevel = 1){
  
  # Define polygon
  cropPolygon <- with(boundingBox,
                      as(raster::extent(list(x = c(p1$x, p2$x), y = c(p1$y, p2$y))), 
                      'SpatialPolygons'
                      )
  )
  
  # Crop
  rasterObject <- raster::crop(rasterObject, cropPolygon)
  
}