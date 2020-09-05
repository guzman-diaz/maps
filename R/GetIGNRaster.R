GetIGNRaster <- function(boundingBox,
                         tile_resolution = 25,
                         ele_folder = '\\\\pocpaco\\maps\\elevation\\'
                         ){
  
  # Get tile numbers
  
  ## Load corner table
  cornerTable <- readRDS(here::here('data', 'cornerData.rds')) %>% 
    dplyr::filter(UTMzone == 30)
  
  ## Filter
  tile_set <- cornerTable %>% 
    dplyr::filter(xmin < boundingBox$p2$x) %>% 
    dplyr::filter(xmax > boundingBox$p1$x) %>% 
    dplyr::filter(ymin < boundingBox$p2$y) %>% 
    dplyr::filter(ymax > boundingBox$p1$y) %>% 
    dplyr::select(name) %>% 
    unlist()
  
  
  # Process
  
  ## Initialize
  rasterObject <- list()
  
  ## Define file name
  if (is.null(ele_folder)){
    ele_folder <- '\\\\pocpaco\\maps\\elevation\\'
  } else {
    ele_folder <- paste(ele_folder, '/', sep = '')
  }
  
  ## Import individual tiles in a list
  for (tile_id in 1:length(tile_set)){
    tile_file <- paste(ele_folder, 
                      'PNOA_MDT',
                      sprintf('%02d', tile_resolution),
                      '_ETRS89_HU30_',
                      sprintf('%04d', tile_set[tile_id]),
                      '_LID.asc', 
                      sep = ''
    )
    
    if (file.exists(tile_file)){
      rasterObject[[tile_id]] <- raster(tile_file)
    } else {
      stop(sprintf('Tile number %04d not found. Go to http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=LIDAR', tile_set[tile_id]))
    }
  }
  
  ## If several tiles, merge
  if (length(rasterObject) > 1){
    rasterObject.merge <- do.call(merge, rasterObject)
  } else {
    rasterObject.merge <- rasterObject[[1]]
  }
  
  
  # Output
  return(rasterObject)
  
  
}