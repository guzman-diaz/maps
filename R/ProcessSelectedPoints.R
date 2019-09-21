ProcessSelectedPoints <- function(trackListL,
                                  pointCoords
){
  
  # Initialize
  ## Summary table
  if (!exists('pointTable', envir = .GlobalEnv)){
    pointTable <- data.frame(lon = as.numeric(),
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
    
    assign('pointTable', pointTable, envir = .GlobalEnv)
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
  
  # Format listened point  
  pointCoords <- t(pointCoords) %>% 
    as.data.frame() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_at(.vars = c('lon', 'lat'), as.numeric)

  # Import tables from global workspace
  pointTable <<- pointTable
  
  proposedTrack <<- proposedTrack
  
  # Define new row from listened point
  newRow <- pointCoords 
  
  # Last row of the existing point table
  if (nrow(pointTable) == 0){
    ## If the table is new, duplicate last and new rows (later remove)
    lastRow <- pointCoords
  } else {
    ## If there are records already, extract last one
    lastRow <- tail(pointTable, 1)
  }
  
  # Add information to new row
  
  if (substr(pointCoords['id'], 1, 3) != substr(lastRow['id'], 1, 3) |
      substr(pointCoords['id'], 1, 3) == 'map'
  ){
    ## Two points not in the same track
    ### Distance
    newRow['dist'] <- geosphere::distm(lastRow[1:2],
                                       newRow[1:2],
                                       fun = distHaversine
    )
    
    ### Elevation
    elevation_diff <- raster::extract(rasterObject,
                                      as.data.frame(TransformCoordinates(pointCoords[1:2], is.lonLat = T))
    ) - raster::extract(rasterObject,
                        as.data.frame(TransformCoordinates(lastRow[1:2], is.lonLat = T))
    )
    
    newRow['gain_pos'] <- max(elevation_diff, 0)
    newRow['gain_neg'] <- min(elevation_diff, 0)
    
    ### Update the proposed track
    proposedTrack <- rbind(proposedTrack, as.data.frame(newRow[c('lon', 'lat')]))

  } else {
    ## Two points in the same track
    ### Get interval of the chunk of records from original track
    pointInterval <- as.numeric(substring(lastRow$id, 5)):as.numeric(substring(newRow$id, 5))
    
    ### Get the in-between original coords
    inBetweenPoints <- trackList[[as.numeric(substr(lastRow$id, 1, 3))]]$table[pointInterval, ]
    
    ### If only one record, duplicate to have two points
    if (nrow(inBetweenPoints) < 2){
      inBetweenPoints <- rbind(inBetweenPoints, inBetweenPoints)
    }
    
    ### Compute all intermediate distances
    for (rowId in 2:nrow(inBetweenPoints)){
      inBetweenPoints[rowId, 'dist'] <- geosphere::distm(inBetweenPoints[rowId-1, c('lon', 'lat')],
                                                         inBetweenPoints[rowId, c('lon', 'lat')],
                                                         fun = distHaversine
      )
      
    }
    
    ### Retain just the sum to incorporate to the summary table
    newRow['dist'] <- sum(inBetweenPoints[-1, 'dist'])
    
    ### Update the proposed track
    # if (as.numeric(newRow['dist']) != 0 | nrow(proposedTrack) == 0){
      proposedTrack <- rbind(proposedTrack, as.data.frame(inBetweenPoints[c('lon', 'lat')]))
    # }

    ### Extract elevation
    for (rowId in 1:nrow(inBetweenPoints)){
      inBetweenPoints[rowId, 'elevation'] <- raster::extract(
        rasterObject,
        as.data.frame(TransformCoordinates(inBetweenPoints[rowId, c('lon', 'lat')], is.lonLat = T))
      )
    }
    
    ### Calculate the elevation gain at each point
    elevation_diff <- diff(inBetweenPoints$elevation)

    ### The result is the sum of positive and negative
    newRow['gain_pos'] <- sum(elevation_diff[elevation_diff >= 0])
    newRow['gain_neg'] <- sum(elevation_diff[elevation_diff <= 0])
  }
  
  # Initialize remaining field, which must be calculated later from the entire table
  newRow[c('cumDist', 'cumGain_pos', 'cumGain_neg')] <- 0
  
  # Remove records with zero distance (duplicates)
  if (as.numeric(newRow['dist']) != 0 | nrow(pointTable) == 0){
    pointTable <- rbind(pointTable, as.data.frame(newRow))
  }

  # Calculate cumulative values
  pointTable %<>%
    dplyr::mutate(cumDist = cumsum(dist),
                  cumGain_pos = cumsum(gain_pos),
                  cumGain_neg = cumsum(gain_neg)
    )
  
  # Export
  assign('pointTable', pointTable, envir =  .GlobalEnv)
  assign('proposedTrack', proposedTrack, envir =  .GlobalEnv)
  
  # Output
  return(pointTable)
  
}