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
    fileName <- paste(folderName, 
                      'PNOA_MDT',
                      sprintf('%02d', tileResolution),
                      '_ETRS89_HU30_',
                      sprintf('%04d', tileSet[tileId]),
                      '_LID.asc', 
                      sep = ''
    )
    
    if (file.exists(fileName)){
      rasterObject[[tileId]] <- raster(fileName)
    } else {
      stop(sprintf('Tile number %04d not found. Go to http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=LIDAR', tileSet[tileId]))
    }
  }
  
  # If several tiles, merge
  if (length(rasterObject) > 1){
    rasterObject.merge <- do.call(merge, rasterObject)
  } else {
    rasterObject.merge <- rasterObject[[1]]
  }
  
  # Add CRS
  crs(rasterObject.merge) <- '+proj=utm +zone=30 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  
  # Output
  return(rasterObject.merge)
  
} 