FindIGNTiles <- function(boundingBox){
  
  # Load corner table
  cornerTable <- readRDS(here::here('data', 'cornerData.rds')) %>% 
    dplyr::filter(UTMzone == 30)
  
  # Create point table to iterate
  pointTable <- with(boundingBox,
       data.frame(x = c(p1$x, p1$x, p2$x, p2$x),
                                y = c(p1$y, p2$y, p1$y, p2$y)
       ))
  
  # Iterate over all points
  tileSet <- vector()
  for (pointId in 1:4){
    
    ## Retrieve point coord
    pointCoord <- pointTable[pointId, ] 
    
    ## Find tile
    tileSet <- cornerTable %>% 
      ### Filter tiles containing the point
      dplyr::filter(xmin <= pointCoord$x & pointCoord$x <= xmax &
                      ymin <= pointCoord$y & pointCoord$y <= ymax
      ) %>% 
      ### Select the tile ids
      dplyr::select(name) %>% 
      ### Combine the result with that of previous points
      union(tileSet) %>% 
      unlist()
  }
  
  return(tileSet)
}