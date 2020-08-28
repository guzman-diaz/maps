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
  tif_raster <- raster::stack(paste(tif_folder, tif_name, sep = '\\'))

  cat(sprintf(' TIF name: %s\n', tif_name))
  cat(sprintf(' TIF no. layers: %d\n ', nlayers(tif_raster)))
  cat(sprintf('TIF CRS: %s\n', raster::crs(tif_raster)))

  ## Mask using shape
  if (go_mask_tif){
    mapShape <- readRDS(paste0(shape_folder, shape_name)) %>% 
      sp::spTransform(CRSobj = crs(tif_raster))
    
    tif_raster <- tif_raster %>% 
      raster::crop(extent(mapShape)) %>% 
      raster::mask(mapShape)
  }
  
  ## Crop to bounding box
  if (go_boundBox){
    tif_raster <- tif_raster %>% 
      raster::crop(c(boundingBox$p1$x, boundingBox$p2$x, boundingBox$p1$y, boundingBox$p2$y))
  }
  
  ## Plot
  if (go_plot_tif){
    raster::plotRGB(tif_raster)
  }
  
  
  ## Transform:
    
  ### Transform each band to array
  tif_matrix_1 <- rayshader::raster_to_matrix(tif_raster[[1]])
  tif_matrix_2 <- rayshader::raster_to_matrix(tif_raster[[2]])
  tif_matrix_3 <- rayshader::raster_to_matrix(tif_raster[[3]])
  
  ### Merge band arrays into one tensor:
  #### Initialize
  tif_matrix <- array(0, dim = c(nrow(tif_matrix_1), ncol(tif_matrix_1), 3))
  
  #### Merge
  tif_matrix[, , 1] <- tif_matrix_1/255 
  tif_matrix[, , 2] <- tif_matrix_2/255 
  tif_matrix[, , 3] <- tif_matrix_3/255
  
  #### Transpose to abide by the elevation raster orientation
  tif_matrix <- aperm(tif_matrix, c(2, 1, 3))

  
  # Elevation raster
  
  ## Load
  ele_raster <- readRDS(here::here('output', 'rasterObject_Asturias.rds'))
  cat(sprintf('Elevation CRS: %s\n', raster::crs(ele_raster)))

  ## Mask
  if (go_mask_elevation){
    mapShape <- readRDS(paste0(shape_folder, shape_name)) %>% 
      sp::spTransform(CRSobj = crs(ele_raster))

    ele_raster <- ele_raster %>% 
      raster::crop(extent(mapShape)) %>% 
      raster::mask(mapShape)
  }
 
  ## Crop by the bounding box
  if (go_boundBox){
    ele_raster <- ele_raster %>% 
      raster::crop(c(boundingBox$p1$x, boundingBox$p2$x, boundingBox$p1$y, boundingBox$p2$y))
    
  }
  
  ## Transform elevation to matrix
  ele_matrix <- rayshader::raster_to_matrix(ele_raster)

  ## Remove NAs (ocean)
  ele_matrix[is.na(ele_matrix)] <- 0
  
  
  # Plot
  plot_3d(tif_matrix, ele_matrix, windowsize = c(1100,900), zscale = 50, shadowdepth = -50,
          zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
  
  
  # Output
  invisible(list(tif_matrix, ele_matrix))
}