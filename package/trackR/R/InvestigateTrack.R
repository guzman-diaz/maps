InvestigateTrack <- function(track_folder = 'C:\\Users\\boss\\Google Drive\\Maps\\plan',
                             track_file = '*',
                             ele_folder = '\\\\pocpaco\\maps\\rasters\\',
                             ele_file = 'AsturiasHD.rds',
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



################################################################################
################################################################################

ProcessSelectedPoints <- function(track_lst,
                                  points_coords,
                                  rasterObject
){

  # ----------------------------------------------------------------------------
  # Initialize

  ## Summary table
  if (!exists('points_tbl', envir = .GlobalEnv)){
    points_tbl <- data.frame(lon = as.numeric(),
                             lat = as.numeric(),
                             id = as.character(),
                             dist = as.numeric(),
                             gain_pos = as.numeric(),
                             gain_neg = as.numeric(),
                             cumDist = as.numeric(),
                             cumGain_pos = as.numeric(),
                             cumGain_neg = as.numeric(),
                             stringsAsFactors = F
    )

    assign('points_tbl', points_tbl, envir = .GlobalEnv)
    print('New table of points initialized.')
  }

  ## Whole track table
  if (!exists('track_proposed', envir = .GlobalEnv)){
    track_proposed <- data.frame(lon = as.numeric(),
                                 lat = as.numeric(),
                                 stringsAsFactors = F
    )

    assign('track_proposed', track_proposed, envir = .GlobalEnv)
    print('New track initialized.')
  }

  # ----------------------------------------------------------------------------
  # Format listened point

  ## In lon-lat format
  points_coords <- t(points_coords) %>%
    as.data.frame() %>%
    mutate_if(is.factor, as.character) %>%
    mutate_at(.vars = c('lon', 'lat'), as.numeric)

  # Import tables from global workspace
  points_tbl <<- points_tbl

  track_proposed <<- track_proposed

  # Define new row from listened point
  row_new <- points_coords

  # Last row of the existing point table
  if (nrow(points_tbl) == 0){
    ## If the table is new, duplicate last and new rows (later remove)
    row_last <- points_coords
  } else {
    ## If there are records already, extract last one
    row_last <- tail(points_tbl, 1)
  }

  # Add information to new row

  if (substr(points_coords['id'], 1, 3) != substr(row_last['id'], 1, 3) |
      substr(points_coords['id'], 1, 3) == 'map'
  ){

    ## -------------------------------------------------------------------------
    ## Two points not in the same track
    ### Distance
    row_new['dist'] <- geosphere::distHaversine(row_last[1:2],
                                                row_new[1:2]
    )

    ### Elevation
    points_coords_utm <- points_coords[1:2]
    points_last_coords_utm <- row_last[1:2]

    #### Transform to SpatialPoints class
    sp::coordinates(points_coords_utm) <- names(points_coords_utm)
    sp::coordinates(points_last_coords_utm) <- names(points_last_coords_utm)

    #### Assign lon-lat CRS: epsg:4326
    sp::proj4string(points_coords_utm) <- sp::CRS('+init=epsg:4326') # lon-lat
    sp::proj4string(points_last_coords_utm) <- sp::CRS('+init=epsg:4326') # lon-lat

    #### Transform to UTM30, i.e. epsg:32630
    points_coords_utm <- sp::spTransform(points_coords_utm, sp::CRS('+init=epsg:32630'))
    points_last_coords_utm <- sp::spTransform(points_last_coords_utm, sp::CRS('+init=epsg:32630'))

    elevation_diff <- raster::extract(rasterObject, points_coords_utm) -
      raster::extract(rasterObject, points_last_coords_utm)

    row_new['gain_pos'] <- max(elevation_diff, 0)
    row_new['gain_neg'] <- min(elevation_diff, 0)

    ### Update the proposed track
    track_proposed <- rbind(track_proposed, as.data.frame(row_new[c('lon', 'lat')]))

  } else {

    ## -------------------------------------------------------------------------
    ## Two points in the same track
    ### Get interval of the chunk of records from original track
    points_interval_idx <- as.numeric(substring(row_last$id, 5)):as.numeric(substring(row_new$id, 5))

    ### Get the in-between original coords
    points_inbetween <- track_lst[[as.numeric(substr(row_last$id, 1, 3))]]@coords[points_interval_idx, ] %>%
      matrix(ncol = 3) %>%
      as.data.frame() %>%
      setNames(c('lon', 'lat', 'z'))

    ### If only one record, duplicate to have two points
    if (nrow(points_inbetween) < 2){
      points_inbetween <- rbind(points_inbetween, points_inbetween)
    }

    ### Compute all intermediate distances
    for (row_id in 2:nrow(points_inbetween)){
      points_inbetween[row_id, 'dist'] <- geosphere::distHaversine(points_inbetween[row_id-1, c('lon', 'lat')],
                                                                   points_inbetween[row_id, c('lon', 'lat')]
      )

    }

    ### Retain just the sum to incorporate to the summary table
    row_new['dist'] <- sum(points_inbetween[-1, 'dist'])

    ### Update the proposed track
    # if (as.numeric(newRow['dist']) != 0 | nrow(track_proposed) == 0){
    track_proposed <- rbind(track_proposed, as.data.frame(points_inbetween[c('lon', 'lat')]))
    # }

    ### Extract elevation
    for (row_id in 1:nrow(points_inbetween)){

      #### Transform to SpatialPoints class
      points_inbetween_utm <- points_inbetween[row_id, c('lon', 'lat')]
      sp::coordinates(points_inbetween_utm) <- names(points_inbetween_utm)

      #### Assign lon-lat CRS: epsg:4326
      sp::proj4string(points_inbetween_utm) <- sp::CRS('+init=epsg:4326') # lon-lat

      #### Transform to UTM30, i.e. epsg:32630
      points_inbetween_utm <- sp::spTransform(points_inbetween_utm, sp::CRS('+init=epsg:32630'))

      points_inbetween[row_id, 'elevation'] <- raster::extract(
        rasterObject,
        as.data.frame(points_inbetween_utm)
      )
    }

    ### Calculate the elevation gain at each point
    elevation_diff <- diff(points_inbetween$elevation)

    ### The result is the sum of positive and negative
    row_new['gain_pos'] <- sum(elevation_diff[elevation_diff >= 0])
    row_new['gain_neg'] <- sum(elevation_diff[elevation_diff <= 0])
  }

  # Initialize remaining field, which must be calculated later from the entire table
  row_new[c('cumDist', 'cumGain_pos', 'cumGain_neg')] <- 0

  # Remove records with zero distance (duplicates)
  if (as.numeric(row_new['dist']) != 0 | nrow(points_tbl) == 0){
    points_tbl <- rbind(points_tbl, as.data.frame(row_new))
  }

  # Calculate cumulative values
  points_tbl %<>%
    dplyr::mutate(cumDist = cumsum(dist),
                  cumGain_pos = cumsum(gain_pos),
                  cumGain_neg = cumsum(gain_neg)
    )

  # Export
  assign('points_tbl', points_tbl, envir =  .GlobalEnv)
  assign('track_proposed', track_proposed, envir =  .GlobalEnv)

  # Output
  return(points_tbl)

}


################################################################################
################################################################################

AssesTrackOnMap <- function(track_lst,
                            graticuleInterval = 0.01,
                            go.showCumulative = T,
                            boundingBox = boundingBox,
                            rasterObject
){

  if (is.data.frame(track_lst)){
    track_lst <- list(list(table = track_lst))
  }


  ui <- fluidPage(
    leafletOutput('myMap'),
    tags$style(type = "text/css", "#myMap {height: calc(100vh - 80px) !important;}"),
    actionButton('ending', 'Done'),
    p()
  )

  server <- function(input, output, session) {

    output$myMap <- renderLeaflet({DisplayOSM(boundingBox,
                                              track_lst = track_lst,
                                              graticuleInterval = 0.1
    )})

    ## Observe "Done" button events
    observeEvent(input$doneButton, {
      stopApp()
    })

    ## Observe clicks on markers of tracks
    observeEvent(input$ending, {
      stopApp()
    })

    observeEvent(input$myMap_marker_click, {
      clickedPoint <- input$myMap_marker_click
      points_coords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = clickedPoint$id)

      ProcessSelectedPoints(track_lst = track_lst, points_coords = points_coords, rasterObject = rasterObject)

      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1
        )
    })

    ## Observe clicks on any point of the map
    observeEvent(input$myMap_click, {
      clickedPoint <- input$myMap_click
      points_coords <- c(lon = clickedPoint$lng, lat = clickedPoint$lat, id = 'map')

      pointTable <- ProcessSelectedPoints(track_lst = track_lst, points_coords = points_coords, rasterObject = rasterObject)

      if (go.showCumulative){
        pointText <- sprintf('%.1f km %.0f m', pointTable$cumDist/1e3, pointTable$cumGain_pos)
      } else {
        pointText <- as.character(nrow(pointTable))
      }

      leafletProxy('myMap') %>%
        addCircles(lng = clickedPoint$lng, lat = clickedPoint$lat, group = 'circles',
                   weight = 1, radius = 5, color = 'black', fillColor = 'green',
                   fillOpacity = 0.2, opacity = 1, label = pointText,
                   labelOptions = labelOptions(noHide = T, direction = 'top')
        )

      print('---------------------------------------')
      print(pointTable)


    })

  }

  shinyApp(ui, server)
}
