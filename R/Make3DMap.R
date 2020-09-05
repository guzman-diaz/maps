Make3DMap <- function(go_boundBox = TRUE,
                      osmType_id = NULL,
                      go_mask_elevation = FALSE,
                      go_mask_tif = FALSE,
                      go_plot_tif = FALSE,
                      tif_folder = '\\\\pocpaco\\maps\\overlays\\',
                      tif_name = 'Asturias_1980_georef.tif',
                      shape_folder = '\\\\pocpaco\\maps\\shapes\\',
                      shape_name = 'shape_Asturias.rds'
){
  
  
  # Preliminaries
  
  ## Load libraries
  pacman::p_load(rayshader)
  pacman::p_load(rgdal)
  pacman::p_load(proj4)
  pacman::p_load(raster)
  pacman::p_load(geoviz)
  pacman::p_load(shiny)
  pacman::p_load(leaflet)
  pacman::p_load(geosphere)
  pacman::p_load(scales)
  
  ## Define OSM urls: http://leaflet-extras.github.io/leaflet-providers/preview/
  osmType_lst <- list()
  osmType_lst[[1]] <- 'https://tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[2]] <- 'https://tile.opentopomap.org/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[3]] <- 'https://tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[4]] <- 'https://tile.thunderforest.com/transport-dark/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[5]] <- 'https://tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[6]] <- 'https://tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[7]] <- 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[8]] <- 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}?apikey=a5e1db75d71d42d8a0c9acf915b1d63b'
  osmType_lst[[9]] <- 'https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png'
  
  
  # Define cropped area manually
  
  if (go_boundBox){
    source(here::here('R', 'SelectMapArea.R'))
    source(here::here('R', 'ShowOSM.R'))
    source(here::here('R', 'DefineBoundingBox.R'))
    source(here::here('R', 'TransformCoordinates.R'))

    SelectMapArea(environment = environment())
    
    ShowOSM(boundingBox, graticuleInterval = 0.1)
  }
  
  
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
  
  
  # Process TIF
  
  ## Load TIF
  
  if (is.null(osmType_id)){
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
    
  } else {

    mapObject <- OpenStreetMap::openmap(upperLeft = c(lat = boundingBox$p2$lat, lon = boundingBox$p1$lon),
                                        lowerRight = c(lat = boundingBox$p1$lat, lon = boundingBox$p2$lon),
                                        zoom = NULL,
                                        type = osmType_lst[[osmType_id]],
                                        mergeTiles = TRUE
    )
    
    ## Georeference the tif raster according to the elevation raster
    tif_raster <- raster::raster(mapObject)
    crs(tif_raster) <- crs(ele_raster)
    extent(tif_raster) <- unlist(extent(ele_raster))
  }
  
  ## Transform:
  
  ### Transform each band to array
  tif_tensor_1 <- rayshader::raster_to_matrix(tif_raster[[1]])
  tif_tensor_2 <- rayshader::raster_to_matrix(tif_raster[[2]])
  tif_tensor_3 <- rayshader::raster_to_matrix(tif_raster[[3]])
  
  ### Merge band arrays into one tensor:
  #### Initialize
  tif_tensor <- array(0, dim = c(nrow(tif_tensor_1), ncol(tif_tensor_1), 3))
  
  #### Merge
  tif_tensor[, , 1] <- tif_tensor_1/255 
  tif_tensor[, , 2] <- tif_tensor_2/255 
  tif_tensor[, , 3] <- tif_tensor_3/255
  
  #### Transpose to abide by the elevation raster orientation
  tif_tensor <- aperm(tif_tensor, c(2, 1, 3))

  
  # Plot
  plot_3d(tif_tensor, ele_matrix - max(ele_matrix), 
          windowsize = c(1100,900), 
          zscale = 50, zoom = 0.65, 
          shadowdepth = -50,
          phi = 45, theta = -45, fov=30, 
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )  
  
  # Output
  invisible(list(tif_tensor = tif_tensor, ele_matrix = ele_matrix))
}