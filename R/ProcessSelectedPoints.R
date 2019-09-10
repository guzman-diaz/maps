ProcessSelectedPoints <- function(trackList = NULL,
                                  pointCoords
){
  
  if (!exists('pointTable', envir = .GlobalEnv)){
    pointTable <- data.frame(lon = as.numeric(),
                             lat = as.numeric(),
                             id = as.character(),
                             dist = as.numeric(),
                             stringsAsFactors = F
    ) %>% 
      setNames(c('lon', 'lat', 'id', 'dist'))
    assign('pointTable', pointTable, envir = .GlobalEnv)
    print('New table of points created.')
  }
  
  # Format listened point  
  pointCoords <- t(pointCoords) %>% 
    as.data.frame() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_at(.vars = c('lon', 'lat'), as.numeric)

  # Import pointTable
  pointTable <<- pointTable

  # Last and new rows
  newRow <- pointCoords 
  
  # Last row of the existing point table
  if (nrow(pointTable) == 0){
    ## If the table is new, duplicate last and new rows (later remove)
    lastRow <- pointCoords
  } else {
    ## If there are records already, extract last one
    if (pointCoords$id != 'map'){
      ### If the point is on a track, remove the duplicated click
      # pointTable <- pointTable[-nrow(pointTable), ]
    }
    lastRow <- tail(pointTable, 1)
  }

  # Add information to new row
  
  if (substr(pointCoords['id'], 1, 3) != substr(lastRow['id'], 1, 3) |
      substr(pointCoords['id'], 1, 3) == 'map'
  ){
    ## Two points not in the same track
    newRow['dist'] <- geosphere::distm(lastRow[1:2],
                                       newRow[1:2],
                                       fun = distHaversine
    )
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

    newRow['dist'] <- sum(inBetweenPoints[-1, 'dist'])
  }
  

  # Remove records with zero distance (duplicates)
  if (as.numeric(newRow['dist']) != 0 | nrow(pointTable) == 0){
    pointTable <- rbind(pointTable, as.data.frame(newRow))
  }
  
  # Export
  assign('pointTable', pointTable, envir =  .GlobalEnv)

  # Output
  return(pointTable)

}