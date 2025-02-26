GetIGNRaster <- function(go_boundBox = TRUE,
                         track_folder = here::here('data', 'tracks', 'redes'),
                         track_file = '*', # NULL if no track is employed
                         tile_resolution = 25,
                         ele_folder = '\\\\pocpaco\\maps\\elevation\\'
                         ){
  
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
  
  ## Transform to UTM30, i.e. epsg:32630
  boundingBox <- as(boundingBox, 'SpatialPolygons')
  sp::proj4string(boundingBox) <- sp::CRS('+init=epsg:4326') # lon-lat projection
  boundingBox <-boundingBox %>% 
    sp::spTransform(sp::CRS('+init=epsg:32630')) %>% 
    raster::extent()
  
  
  # ============================================================================
  # Get tile numbers

  ## Load corner table
  cornerTable <- readRDS(here::here('data', 'cornerData.rds')) %>% 
    dplyr::filter(UTMzone == 30)
  
  ## Filter
  tile_set <- cornerTable %>% 
    dplyr::filter(xmin < boundingBox@xmax) %>% 
    dplyr::filter(xmax > boundingBox@xmin) %>% 
    dplyr::filter(ymin < boundingBox@ymax) %>% 
    dplyr::filter(ymax > boundingBox@ymin) %>% 
    dplyr::select(name) %>% 
    unlist()

  
  # ============================================================================
  # Check if file exist

  
  for (tile_id in 1:length(tile_set)){
    tile_file <- paste(ele_folder, 
                       'PNOA_MDT',
                       sprintf('%02d', tile_resolution),
                       '_ETRS89_HU30_',
                       sprintf('%04d', tile_set[tile_id]),
                       '_LID.asc', 
                       sep = ''
    )
    missing_file <- TRUE
    while (missing_file) {
      if (file.exists(tile_file)){
        missing_file <- FALSE
      } else {
        readline(prompt = sprintf('Tile number %04d not found. Go to http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=LIDAR', tile_set[tile_id]))
      }
    }
  }
    

  # ============================================================================
  # Process
  
  ## Initialize
  rasterObject <- list()
  
  ## Import individual tiles in a list
  for (tile_id in 1:length(tile_set)){
    tile_file <- paste(ele_folder, 
                      'PNOA_MDT',
                      sprintf('%02d', tile_resolution),
                      '_ETRS89_HU30_',
                      sprintf('%04d', tile_set[tile_id]),
                      '_LID.asc', 
                      sep = ''
    )
    cat(sprintf('Rastering tile number %04d\n', tile_set[tile_id]))
    rasterObject[[tile_id]] <- raster::raster(tile_file) %>% 
      raster::crop(boundingBox)
  }

  ## If several tiles, merge
  if (length(rasterObject) > 1){
    cat(sprintf('Merging rasters\n'))
    rasterObject_merge <- do.call(raster::merge, rasterObject)
  } else {
    rasterObject_merge <- rasterObject[[1]]
  }
  
  # ## Crop
  # cat(sprintf('Cropping raster\n'))
  # rasterObject_merge <- raster::crop(rasterObject_merge, boundingBox)
  # 
  
  # ============================================================================
  # Output
  return(rasterObject_merge)
  
  
}