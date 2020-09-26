InvestigateTrack <- function(go_boundBox = TRUE,
                             track_folder = here::here('data', 'tracks', 'redes'),
                             track_file = '*' # NULL if no track is employed
){
  
  pacman::p_load(rayshader)
  pacman::p_load(rgdal)
  pacman::p_load(proj4)
  pacman::p_load(raster)
  pacman::p_load(geoviz)
  pacman::p_load(shiny)
  pacman::p_load(leaflet)
  pacman::p_load(geosphere)
  pacman::p_load(psyosphere)
  
  

  # ============================================================================
  # Source files
  source(here::here('R', 'SelectMapArea.R'))
  source(here::here('R', 'DisplayOSM.R'))
  source(here::here('R', 'TransformCoordinates.R'))
  
  
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
  
  
}