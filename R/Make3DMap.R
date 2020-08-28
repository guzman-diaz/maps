Make3DMap <- function(go_cropMap = FALSE
  
){
  # Preliminaries
  
  pacman::p_load(rayshader)
  pacman::p_load(rgdal)
  pacman::p_load(proj4)
  pacman::p_load(raster)
  pacman::p_load(geoviz)
  pacman::p_load(shiny)
  pacman::p_load(leaflet)
  pacman::p_load(geosphere)
  pacman::p_load(scales)
  

  # Define cropped area manually
  if (go_cropMap){
    source(here::here('R', 'SelectMapArea.R'))
    source(here::here('R', 'ShowOSM.R'))
    source(here::here('R', 'DefineBoundingBox.R'))
    source(here::here('R', 'TransformCoordinates.R'))

    SelectMapArea()
    
    ShowOSM(boundingBox, graticuleInterval = 0.1)
  }
  
}