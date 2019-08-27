FindIGNTiles <- function(pointTable, is.lonLat = TRUE){
  
  # Load corner table
  cornerTable <- readRDS(here::here('data', 'cornerData.rds')) %>% 
    dplyr::filter(UTMzone == 30)
  
  # Iterate over all points
  tileId <- vector()
  for (pointId in 1:nrow(pointTable)){
    
    ## Retrieve point coord
    pointCoord <- pointTable[pointId, ] 
    
    ## Convert to xy if lonlat
    if (is.lonLat){
      pointCoord <- TransformCoordinates(pointCoord, is.lonLat = T)
    }
    
    ## Find tile
    tileId <- cornerTable %>% 
      ### Filter tiles containing the point
      dplyr::filter(xmin <= pointCoord$x & pointCoord$x <= xmax &
                      ymin <= pointCoord$y & pointCoord$y <= ymax
      ) %>% 
      ### Select the tile ids
      dplyr::select(name) %>% 
      ### Combine the result with that of previous points
      union(tileId) %>% 
      unlist()
  }
  
  return(tileId)
}