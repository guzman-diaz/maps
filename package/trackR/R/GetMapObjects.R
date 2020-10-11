GetMapObjects <- function(go_boundBox = TRUE,
                          osmType_id = NULL,
                          go_mask_elevation = FALSE,
                          go_mask_tif = FALSE,
                          ele_folder = '\\\\pocpaco\\maps\\rasters\\',
                          ele_file = 'Asturias.rds',
                          track_folder = 'C:\\Users\\boss\\Google Drive\\Maps\\plan',
                          track_file = '*', # NULL if no track is employed
                          tif_folder = '\\\\pocpaco\\maps\\overlays\\',
                          tif_name = 'Asturias_1980_georef.tif',
                          shape_folder = '\\\\pocpaco\\maps\\shapes\\',
                          shape_name = 'shape_Asturias.rds'
){
  

  # ============================================================================
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
  
  
  # ============================================================================
  # Tracks
  
  if (!is.null(track_file)){
    ## Get the kmz file name in the folder path
    if (track_file == '*'){
      track_file <- list.files(track_folder, pattern = '*.kmz', full.names = FALSE)
    }
    
    ## Unzip each KMZ file 
    track_lst <- lapply(track_file, 
                        function(x) 
                          maptools::getKMLcoordinates(kmlfile = unzip(zipfile = file.path(track_folder, x),
                                                                      exdir = here::here('tmp')
                          ), 
                          ignoreAltitude = FALSE)
    )
    
    ## If more than one track was found unbundled in the folder, flatten the list
    if (length(track_lst) > 1){
      track_lst <- unlist(track_lst, recursive = FALSE)
      
      ## Remove odd elements (useless information)
      track_lst <- lapply(track_lst, function(x) {if (nrow(x) == 1) {NULL} else {x}})
      track_lst[sapply(track_lst, is.null)] <- NULL
    } else {
      track_lst <- unlist(track_lst, recursive = FALSE)
      track_lst <- lapply(track_lst, function(x) {if (nrow(x) == 1) {NULL} else {x}})
      track_lst[sapply(track_lst, is.null)] <- NULL
    }
    
    ## Convert elements to data frames and name them
    track_lst <- lapply(track_lst,
                        function(x){
                          x <- as.data.frame(x)
                          names(x) <- c('x', 'y', 'z')
                          ### Convert to SP object
                          sp::coordinates(x) <- names(x)
                          ### Assign lonlat CRS
                          sp::proj4string(x) <- sp::CRS('+init=epsg:4326') # lon-lat
                          return(x)
                        }
    )
    
    
    ## Calculate bbox and show
    ### Bind all tables
    track_tbl <- lapply(track_lst, function(x) as.data.frame(x@coords)) %>% 
      dplyr::bind_rows()
    sp::coordinates(track_tbl) <- names(track_tbl)
    sp::proj4string(track_tbl) <- sp::CRS('+init=epsg:4326') # lon-lat projection
    
    ### Show result
    DisplayOSM(mapObject_sp = track_tbl, graticuleInterval = 0.1, track_lst = track_lst)
  } else {
    track_lst <- NA
  }
  
  # Define bounding box from tracks
  boundingBox <- track_tbl %>% 
    ### Calculate extent
    raster::extent()
  
  
  # ============================================================================
  # Process bounding box
  
  if (!exists('boundingBox')){
    boundingBox <- NULL
  }
  
  ## Redefine bounding box using map selection
  if (go_boundBox){
    SelectMapArea(environment = environment(), boundingBox = boundingBox)
    
    DisplayOSM(boundingBox, graticuleInterval = 0.1)
  }
  
  ## Transform to UTM30, i.e. epsg:32630
  boundingBox <- as(boundingBox, 'SpatialPolygons')
  sp::proj4string(boundingBox) <- sp::CRS('+init=epsg:4326') # lon-lat projection
  boundingBox <-boundingBox %>% 
    sp::spTransform(sp::CRS('+init=epsg:32630')) %>% 
    raster::extent()
  

  # ============================================================================
  # Elevation
  
  ## Load
  ele_raster <- readRDS(file.path(ele_folder, ele_file))
  
  if (!is.null(boundingBox)){
    ele_raster <- raster::crop(ele_raster, boundingBox)
  }
  
  cat(sprintf('Elevation name: %s \n', ele_file))
  cat(sprintf('Elevation CRS: %s\n', raster::crs(ele_raster)))

  ## Mask if requested
  if (go_mask_elevation){
    mapShape <- readRDS(paste0(shape_folder, shape_name)) %>% 
      ### Assign CRS to the elevation raster CRS
      sp::spTransform(CRSobj = crs(ele_raster))
    
    ele_raster <- ele_raster %>% 
      raster::crop(extent(mapShape)) %>% 
      raster::mask(mapShape) %>% 
      raster::crop(ele_raster, boundingBox)
  }
  
  ## If there is a track, recalculate elevation from raster
  track_lst <- lapply(track_lst, 
                      function(x){
                        #### Transform to UTM30, i.e. epsg:32630
                        points_coords_utm <- sp::spTransform(x, sp::CRS('+init=epsg:32630'))

                        #### Get elevation
                        x$elevation <- raster::extract(ele_raster, points_coords_utm)
                        return(x)
                      }
  )
  
  
  # ============================================================================
  # TIF raster
  
  if (is.null(osmType_id)){
    
    ## Load a georeferenced TIF from disk
    tif_raster <- raster::stack(file.path(tif_folder, tif_name))
    
    if (!is.null(boundingBox)){
      tif_raster <- raster::crop(tif_raster, boundingBox)
    }
    
    cat(sprintf('TIF name: %s\n', tif_name))
    cat(sprintf('TIF no. layers: %d\n', nlayers(tif_raster)))
    cat(sprintf('TIF CRS: %s\n', raster::crs(tif_raster)))
    
    ## Mask using shape
    if (go_mask_tif){
      mapShape <- readRDS(file.path(shape_folder, shape_name)) %>% 
        sp::spTransform(CRSobj = crs(tif_raster))
      
      tif_raster <- tif_raster %>% 
        raster::crop(extent(mapShape)) %>% 
        raster::mask(mapShape) %>% 
        raster::crop(with(boundingBox, c(p1$x, p2$x, p1$y, p2$y)))
    }
    
    ## Crop to bounding box
    if (is.null(boundingBox)){
      tif_raster <- tif_raster %>% 
        raster::crop(boundingBox)
    }
    
  } else {
    ## Load map from OSM
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
  
  ## Plot
  plotRGB(tif_raster)
  
  
  # ============================================================================
  # Raster to matrix conversion
  
  ## Elevation raster
  ele_matrix <- rayshader::raster_to_matrix(ele_raster, verbose = FALSE)
  cat(sprintf('Size of the elevation matrix: %d x %d\n', dim(ele_matrix)[1], dim(ele_matrix)[2]))
  
  ### Remove NAs (ocean)
  ele_matrix[is.na(ele_matrix)] <- 0
  
  ## TIF raster
  ### Transform each band to array
  tif_tensor_1 <- rayshader::raster_to_matrix(tif_raster[[1]], verbose = FALSE)
  tif_tensor_2 <- rayshader::raster_to_matrix(tif_raster[[2]], verbose = FALSE)
  tif_tensor_3 <- rayshader::raster_to_matrix(tif_raster[[3]], verbose = FALSE)
  
  ## Merge band arrays into one tensor:
  ### Initialize
  tif_tensor <- array(0, dim = c(nrow(tif_tensor_1), ncol(tif_tensor_1), 3))
  
  ### Merge
  tif_tensor[, , 1] <- tif_tensor_1/255 
  tif_tensor[, , 2] <- tif_tensor_2/255 
  tif_tensor[, , 3] <- tif_tensor_3/255
  
  ### Transpose to abide by the elevation raster orientation
  tif_tensor <- aperm(tif_tensor, c(2, 1, 3))
  cat(sprintf('Size of the TIF tensor: %d x %d x %d\n', 
              dim(tif_tensor)[1], dim(tif_tensor)[2], dim(tif_tensor)[3]
  ))

  
  # ============================================================================
  # Output
  return(list(
    track_lst = track_lst,
    ele_matrix = ele_matrix,
    ele_raster = ele_raster,
    tif_tensor = tif_tensor
  ))
}
