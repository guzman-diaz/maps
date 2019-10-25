InvestigateTrack <- function(trackContainerFolder,
                             go.useStoredRaste = TRUE
                             ){
  
  pacman::p_load(rgdal)
  pacman::p_load(proj4)
  pacman::p_load(raster)
  pacman::p_load(geoviz)
  pacman::p_load(shiny)
  pacman::p_load(leaflet)
  pacman::p_load(maptools)

  source(here::here('R', 'DefineBoundingBox.R'))  
  source(here::here('R', 'ShowOSM.R'))  
  source(here::here('R', 'TransformCoordinates.R'))  
  source(here::here('R', 'FindIGNTiles.R'))  
  source(here::here('R', 'ImportIGNTiles.R'))  
  source(here::here('R', 'CropRaster.R'))  
  source(here::here('R', 'AssesTrackOnMap.R'))  
  

  # Load all tracks in the folder  
  ## List the kmz files in a given folder path
  kmzFileNames <- list.files(here::here('data', 'tracks', trackContainerFolder), 
                             pattern = '*.kmz', 
                             full.names = FALSE
  )
  
  ## Unzip each KMZ file 
  trackList <- lapply(kmzFileNames, 
                      function(x) 
                        maptools::getKMLcoordinates(kmlfile = unzip(zipfile = paste0(here::here('data', 'tracks', trackContainerFolder), '/', x),
                                                                    exdir   = here::here('data', 'tracks', 'out')
                        ), 
                        ignoreAltitude = FALSE)
  )
  
  ## If more than one track was found unbundled in the folder, flatten the list
  if (length(trackList) > 1){
    trackList <- unlist(trackList, recursive = FALSE)
  }
  
  ## Remove odd elements (useless information)
  trackList <- trackList[[1]][seq(2, length(trackList[[1]]), 2)]
  
  ## Format list elements
  trackList <- lapply(trackList, function(x) {
    x <- list(table = data.frame(lon = x[, 1], lat = x[, 2]),
              elevation = x[, 3]
    )
  })
  
  
  # Calculate bbox and show
  boundingBox <- DefineBoundingBox(trackList = trackList, zoomLevel = 1.1)
  
  ShowOSM(boundingBox, graticuleInterval = 0.1, trackList = trackList)
  
  
  # Get tiles from IGN
  
  ## Obtain tile codes
  tileSet <- FindIGNTiles(boundingBox = boundingBox)
  cat('Tile numbers: ')
  cat(tileSet)
  cat('\n')
  
  ## Import data into raster objects
  if (go.useStoredRaste){
    ### Previously saved raster
    cat('Loading raster object from disk...')
    rasterObject <- readRDS(here::here('data', 'tracks', trackContainerFolder, 'rasterObject.rds'))
    cat('finished\n')
  } else {
    ### Download 
    cat('Creating raster object...')
    rasterObject <- ImportIGNTiles(tileSet, 
                                   tileResolution = 25,
                                   folderName = NULL # from NAS \\pocpaco\maps\
    )
    
    ### Crop
    rasterObject <- CropRaster(rasterObject, boundingBox)
    
    ### Save raster
    saveRDS(rasterObject, here::here('data', 'tracks', trackContainerFolder, 'rasterObject.rds'))
    cat('finished\n')
  }
  
  
  # Select points
  rm(pointTable, envir = .GlobalEnv)
  rm(proposedTrack, envir = .GlobalEnv)
  
  AssesTrackOnMap(trackList = trackList, boundingBox = boundingBox, rasterObject = rasterObject)

}