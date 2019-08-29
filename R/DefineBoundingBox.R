DefineBoundingBox <- function(boundingBox = NULL, 
                              p1 = NULL, 
                              p2 = NULL, 
                              is.lonLat = T,
                              zoomLevel = 1
){
  
  # Gather in a list
  if (is.null(boundingBox)){
    boundingBox <- list(p1 = as.list(p1), p2 = as.list(p2))
  }
  
  # Check if xy coordinates are present. If not, retrieve
  if (is.lonLat){
    boundingBox$p1 <-  c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = T))
    boundingBox$p2 <-  c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = T))
  }
  
  # Apply zoom
  ## Diagonal size (in meters)
  boundingBoxDiagonal <- with(boundingBox,
                              sqrt((p2$x-p1$x)^2+(p2$y-p1$y)^2)
  )
  
  ## Modify bounding box coords
  boundingBox$p1$x <- boundingBox$p1$x + (1-zoomLevel)*boundingBoxDiagonal
  boundingBox$p1$y <- boundingBox$p1$y + (1-zoomLevel)*boundingBoxDiagonal
  boundingBox$p2$x <- boundingBox$p2$x - (1-zoomLevel)*boundingBoxDiagonal
  boundingBox$p2$y <- boundingBox$p2$y - (1-zoomLevel)*boundingBoxDiagonal
  
  # Output
  return(boundingBox)
  
}