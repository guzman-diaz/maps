InvestigateTrack <- function(track_folder = here::here('data', 'tracks', 'plan'),
                             track_file = '*',
                             ele_folder = '\\\\pocpaco\\maps\\rasters\\',
                             ele_file = 'Asturias.rds',
                             go_startOver = TRUE
                             
){
  

  # ============================================================================

  pacman::p_load(shiny)
  
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
    SelectMapArea(environment = environment(), boundingBox = boundingBox)
    DisplayOSM(boundingBox, graticuleInterval = 0.1)
  }
  
  
  # ============================================================================
  # Load raster

  ele_raster <- readRDS(file.path(ele_folder, ele_file))
  

  # ============================================================================
  # Trace track on map
  
  if (go_startOver){
    suppressWarnings(
      rm('points_tbl', 'track_proposed', envir = .GlobalEnv)
    )
  }
  
  AssesTrackOnMap(track_lst = if (exists('track_lst')) {track_lst} else {NULL},
                  boundingBox = boundingBox,
                  rasterObject = ele_raster
  )
  
  
}