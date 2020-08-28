Make3DMap <- function(go_cropMap = FALSE,
                      tif_folder = '\\\\pocpaco\\maps\\overlays\\',
                      tif_name = 'Asturias_geologico_georef.tif'
  
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
  
  # Load geoTIF:
  tifData_raster <- raster::stack(paste(tif_folder, tif_name, sep = '\\'))
  cat(sprintf(' TIF name: %s\n', tif_name))
  
  # How many bands?
  cat(sprintf(' TIF no. layers: %d\n ', nlayers(tifData_raster)))
  
  # Which coordinate reference system?
  cat(sprintf('TIF CRS: %s\n', raster::crs(tifData_raster)))

  
}