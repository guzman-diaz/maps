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
  if (!exists('proposedTrack', envir = .GlobalEnv)){
    proposedTrack <- data.frame(lon = as.numeric(),
                                lat = as.numeric(),
                                stringsAsFactors = F
    ) 
    
    assign('proposedTrack', proposedTrack, envir = .GlobalEnv)
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
    ## Two points in the same track
    ### Get interval of the chunk of records from original track
    pointInterval <- as.numeric(substring(row_last$id, 5)):as.numeric(substring(row_new$id, 5))
    
    ### Get the in-between original coords
    points_inBetween <- track_lst[[as.numeric(substr(row_last$id, 1, 3))]]$table[pointInterval, ]
    
    ### If only one record, duplicate to have two points
    if (nrow(points_inBetween) < 2){
      points_inBetween <- rbind(points_inBetween, points_inBetween)
    }
    
    ### Compute all intermediate distances
    for (row_id in 2:nrow(points_inBetween)){
      points_inBetween[row_id, 'dist'] <- geosphere::distHaversine(points_inBetween[row_id-1, c('lon', 'lat')],
                                                                 points_inBetween[row_id, c('lon', 'lat')]
      )
      
    }
    
    ### Retain just the sum to incorporate to the summary table
    row_new['dist'] <- sum(points_inBetween[-1, 'dist'])
    
    ### Update the proposed track
    # if (as.numeric(newRow['dist']) != 0 | nrow(proposedTrack) == 0){
    track_proposed <- rbind(track_proposed, as.data.frame(points_inBetween[c('lon', 'lat')]))
    # }
    
    
    ############### TODO: transform in-between coords to utm to extract elevation
    ### Extract elevation
    for (row_id in 1:nrow(points_inBetween)){
      points_inBetween[row_id, 'elevation'] <- raster::extract(
        rasterObject,
        as.data.frame(TransformCoordinates(points_inBetween[row_id, c('lon', 'lat')], is.lonLat = T))
      )
    }
    
    ### Calculate the elevation gain at each point
    elevation_diff <- diff(points_inBetween$elevation)
    
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
  assign('proposedTrack', track_proposed, envir =  .GlobalEnv)
  
  # Output
  return(points_tbl)
  
}