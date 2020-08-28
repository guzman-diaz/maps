Make3DMap <- function(go_boundBox = FALSE,
                      go_mask_elevation = TRUE,
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
  
  ## Plot
  if (go_plot_tif){
    raster::plotRGB(tifData_raster)
  }
  
  
  ## Transform:
    
  ### Transform each band to array
  tifData_matrix_1 <- rayshader::raster_to_matrix(tifData_raster[[1]])
  tifData_matrix_2 <- rayshader::raster_to_matrix(tifData_raster[[2]])
  tifData_matrix_3 <- rayshader::raster_to_matrix(tifData_raster[[3]])
  
  ### Merge band arrays into one tensor:
  #### Initialize
  tifData_matrix_all <- array(0, dim = c(nrow(tifData_matrix_1), ncol(tifData_matrix_1), 3))
  
  #### Merge
  tifData_matrix_all[, , 1] <- tifData_matrix_1/255 
  tifData_matrix_all[, , 2] <- tifData_matrix_2/255 
  tifData_matrix_all[, , 3] <- tifData_matrix_3/255
  
  #### Transpose to abide by the elevation raster orientation
  tifData_matrix_all <- aperm(tifData_matrix_all, c(2, 1, 3))

  
  # Elevation raster
  
  ## Load
  elevationData_raster <- readRDS(here::here('output', 'rasterObject_Asturias.rds'))
  cat(sprintf('Elevation CRS: %s\n', raster::crs(elevationData_raster)))

  ## Mask
  if (go_mask_elevation){
    mapShape <- readRDS(paste0(shape_folder, shape_name)) %>% 
      sp::spTransform(CRSobj = crs(elevationData_raster))

    elevationData_raster <- elevationData_raster %>% 
      raster::crop(extent(mapShape)) %>% 
      raster::mask(mapShape)
  }
 
  ## Crop by the bounding box
  if (go_boundBox){
    elevationData_raster <- elevationData_raster %>% 
      raster::crop(c(boundingBox$p1$x, boundingBox$p2$x, boundingBox$p1$y, boundingBox$p2$y))
    
  }
  
}