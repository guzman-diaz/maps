DefineBoundingBox <- function(pointTable = NULL,
                              trackList = NULL,
                              boundingBox = NULL, 
                              p1 = NULL, 
                              p2 = NULL, 
                              is.lonLat = T,
                              rasterObject = NULL,
                              zoomLevel = 1
){
  
  # From track list
  if (!is.null(trackList)){
    ## Bind all tables
    lonLat.corners <- lapply(trackList, function(x) {x <- x$table}) %>% 
      dplyr::bind_rows() %>% 
      summarize(max.lon = max(lon), max.lat = max(lat), min.lon = min(lon), min.lat = min(lat))
    
    ## Define bbox
    p1 = c(lon = lonLat.corners$min.lon, lat = lonLat.corners$min.lat) 
    p2 = c(lon = lonLat.corners$max.lon, lat = lonLat.corners$max.lat) 
  }
  
  
  # From raster
  if (!is.null(rasterObject)){
    p1 <- c(x = rasterObject@extent@xmin, y = rasterObject@extent@ymin)
    p2 <- c(x = rasterObject@extent@xmax, y = rasterObject@extent@ymax)
    is.lonLat <- F
  }
  
  # Create bounding box from lonlat point table
  if (!is.null(pointTable)){
    p1 <- c(lon = min(pointTable[, 1]), lat = min(pointTable[, 2]))
    p2 <- c(lon = max(pointTable[, 1]), lat = max(pointTable[, 2]))
  }
  
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
  
  # Redefine latlon from zoomed xy coordinates
  boundingBox$p1[c('lon', 'lat')] <- TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = F)
  boundingBox$p2[c('lon', 'lat')] <- TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = F)
  
  # Output
  return(boundingBox)
  
}