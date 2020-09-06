GetMapObjects <- function(go_boundBox = TRUE,
                          osmType_id = NULL,
                          go_mask_elevation = FALSE,
                          go_mask_tif = FALSE,
                          ele_folder = here::here('data'),
                          ele_raster = 'rasterObject_Asturias.rds',
                          track_folder = here::here('data', 'tracks', 'hibeo'),
                          track_file = '*' # NULL if no track is employed
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
  
  ## Source files
  source(here::here('R', 'SelectMapArea.R'))
  source(here::here('R', 'ShowOSM.R'))
  source(here::here('R', 'TransformCoordinates.R'))
  
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
    
    # Format list elements
    track_lst <- lapply(track_lst, function(x) {
      x <- list(table = data.frame(lon = x[, 1], lat = x[, 2], elevation = x[, 3]))
    })
    
    ## Calculate bbox and show
    ### Bind all tables
    lonlat_corners <- lapply(track_lst, function(x) {x <- x$table}) %>% 
      dplyr::bind_rows() %>% 
      summarize(max.lon = max(lon), max.lat = max(lat), min.lon = min(lon), min.lat = min(lat))
    
    ### Define bbox
    p1 = c(lon = lonlat_corners$min.lon, lat = lonlat_corners$min.lat) 
    p2 = c(lon = lonlat_corners$max.lon, lat = lonlat_corners$max.lat) 
    
    boundingBox <- list(p1 = as.list(p1), p2 = as.list(p2))
    
    boundingBox$p1 <-  c(boundingBox$p1, TransformCoordinates(as.vector(boundingBox$p1), is.lonLat = T))
    boundingBox$p2 <-  c(boundingBox$p2, TransformCoordinates(as.vector(boundingBox$p2), is.lonLat = T))
    
    ### Show result
    ShowOSM(boundingBox, graticuleInterval = 0.1, trackList = track_lst)
  }
  

  
  # ============================================================================
  # Bounding box
  if (!exists('boundingBox')){
    boundingBox <- NULL
  }
  
  if (go_boundBox){
    SelectMapArea(environment = environment(), boundingBox = boundingBox)
    
    ShowOSM(boundingBox, graticuleInterval = 0.1)
  }
  
  
  # ============================================================================
  # Elevation raster
  
  ## Load
  ele_raster <- readRDS(file.path(ele_folder, ele_raster))
  cat(sprintf('Elevation CRS: %s\n', raster::crs(ele_raster)))
  
}
