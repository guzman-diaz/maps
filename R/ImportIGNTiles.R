ImportIGNTiles <- function(tileSet,
                           tileResolution = 25,
                           folderName = NULL
){
  
  pacman::p_load(raster)
  
  # Initialize
  rasterObject <- list()
  
  # Define file name
  if (is.null(folderName)){
    folderName <- '\\\\pocpaco\\maps\\'
  } else {
    folderName <- paste(folderName, '/', sep = '')
  }

  # Import individual tiles in a list
  for (tileId in 1:length(tileSet)){
    rasterObject[[tileId]] <- raster(paste(folderName, 
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
  
  # Output
  return(rasterObject.merge)
  
} 