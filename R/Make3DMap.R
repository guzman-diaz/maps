Make3DMap <- function(go_boundBox = FALSE,
                      go_mask_elevation = FALSE,
                      go_mask_tif = FALSE,
                      tif_folder = '\\\\pocpaco\\maps\\overlays\\',
                      tif_name = 'Asturias_1980_georef.tif',
                      shape_folder = '\\\\pocpaco\\maps\\shapes\\',
                      shape_name = 'shape_Asturias.rds',
                      go_plot_tif = TRUE
                      
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
  if (go_boundBox){
    source(here::here('R', 'SelectMapArea.R'))
    source(here::here('R', 'ShowOSM.R'))
    source(here::here('R', 'DefineBoundingBox.R'))
    source(here::here('R', 'TransformCoordinates.R'))

    SelectMapArea(environment = environment())
    
    ShowOSM(boundingBox, graticuleInterval = 0.1)
  }
  
  
  # Process TIF
  
  ## Load TIF
  tifData_raster <- raster::stack(paste(tif_folder, tif_name, sep = '\\'))

  cat(sprintf(' TIF name: %s\n', tif_name))
  cat(sprintf(' TIF no. layers: %d\n ', nlayers(tifData_raster)))
  cat(sprintf('TIF CRS: %s\n', raster::crs(tifData_raster)))

  ## Mask using shape
  if (go_mask_tif){
    mapShape <- readRDS(paste0(shape_folder, shape_name)) %>% 
      sp::spTransform(CRSobj = crs(tifData_raster))
    
    tifData_raster <- tifData_raster %>% 
      raster::crop(extent(mapShape)) %>% 
      raster::mask(mapShape)
  }
  
  ## Crop to bounding box
  if (go_boundBox){
    tifData_raster <- tifData_raster %>% 
      raster::crop(c(boundingBox$p1$x, boundingBox$p2$x, boundingBox$p1$y, boundingBox$p2$y))
  }
  
  if (go_plot_tif){
    raster::plotRGB(tifData_raster)
  }
  
}