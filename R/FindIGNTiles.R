FindIGNTiles <- function(boundingBox){
  
  # Load corner table
  cornerTable <- readRDS(here::here('data', 'cornerData.rds')) %>% 
    dplyr::filter(UTMzone == 30)
  
  # Filter
  tileSet <- cornerTable %>% 
    dplyr::filter(xmin < boundingBox$p2$x) %>% 
    dplyr::filter(xmax > boundingBox$p1$x) %>% 
    dplyr::filter(ymin < boundingBox$p2$y) %>% 
    dplyr::filter(ymax > boundingBox$p1$y) %>% 
    dplyr::select(name) %>% 
    unlist()
 
  return(tileSet)
}