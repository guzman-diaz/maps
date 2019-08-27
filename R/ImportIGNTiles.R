ImportIGNTiles <- function(tileSet,
                           tileResolution = 25
){
  
  pacman::p_load(raster)
  
  # Initialize
  rasterObject <- list()
  
  # Import individual tiles in a list
  for (tileId in seq(tileSet)){
    rasterObject[[tileId]] <- raster(paste('\\\\pocpaco\\maps\\', 
                                           'PNOA_MDT',
                                           tileResolution,
                                           '_ETRS89_HU30_',
                                           sprintf('%04d', tileSet[tileId]),
                                           '_LID.asc', 
                                           sep = ''
    ))
  }
  
  # If several tiles, merge
  if (length(rasterObject) > 1){
    rasterObject.merge <- do.call(merge, rasterObject)
  } else {
    rasterObject.merge <- rasterObject[[1]]
  }
  
  # Convert raster object to matrix
  mapMatrix <- matrix(raster::extract(rasterObject.merge, 
                                      extent(rasterObject.merge), 
                                      buffer = 1000
  ),
  nrow = ncol(rasterObject.merge), 
  ncol = nrow(rasterObject.merge)
  )
  
  # Bounding box
  ## xy
  boundingBox <- list(
    p1 = as.list(c(x = extent(rasterObject.merge)@xmin, y = extent(rasterObject.merge)@ymin)),
    p2 = as.list(c(x = extent(rasterObject.merge)@xmax, y = extent(rasterObject.merge)@ymax))
  )
  
  ## lonlat
  boundingBox$p1 <-  c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = F))
  boundingBox$p2 <-  c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = F))
  
  # Output
  return(list(elevation = mapMatrix,
              extent = extent(rasterObject.merge)
  ))
} 