TransformCoordinates <- function(coordHorVer, is.lonLat = TRUE, UTMzone = 30){
  pacman::p_load(magrittr)
  pacman::p_load(rgdal)
  
  proj4string <- paste('+proj=utm +zone=',
                       UTMzone,
                       ' +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                       sep =''
  )
  
  if (is.lonLat){
    transformedCoord <- project(xy = c(coordHorVer[1], coordHorVer[2]), 
                                proj = proj4string, 
                                inverse = FALSE
    ) %>% 
      as.data.frame() %>% 
      setNames(c('x', 'y'))
  } else {
    transformedCoord <- project(xy = c(coordHorVer[1], coordHorVer[2]), 
                                proj = proj4string, 
                                inverse = TRUE
    ) %>% 
      as.data.frame() %>% 
      setNames(c('lon', 'lat'))
  }
  
  return(transformedCoord)
}
