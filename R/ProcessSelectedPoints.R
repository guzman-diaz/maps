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
    print('New table created')
  }
  
  # Format listened point  
  pointCoords <- t(pointCoords) %>% 
    as.data.frame() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_at(.vars = c('lon', 'lat'), as.numeric)

  # Import pointTable
  pointTable <<- pointTable
  # print(pointTable)

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
    print('diferente')
  } else {
    ## Two points in the same track
    newRow['dist'] <- geosphere::distm(lastRow[1:2],
                                       newRow[1:2],
                                       fun = distHaversine
    )
    print('mismo track')
  }
  

  # Remove records with zero distance (duplicates)
  if (as.numeric(newRow['dist']) != 0 | nrow(pointTable) == 0){
    pointTable <- rbind(pointTable, as.data.frame(newRow))
  }
  
  # #
  # for (rowId in 2:nrow(pointTable)){
  #   ### When two consecutive points have the same trackId
  #   if (pointTable[rowId, 'trackId'] != 'map' & 
  #       pointTable[rowId, 'trackId'] == pointTable[(rowId-1), 'trackId']
  #   ){
  #     #### Get chunk of records from original track
  #     pointInterval <- pointTable[rowId-1, 'pointId']:pointTable[rowId, 'pointId']
  #     newRows <- trackList[[as.numeric(pointTable[rowId, 'trackId'])]]$table[pointInterval, ] %>% 
  #       dplyr::mutate(trackId = pointTable[rowId, 'trackId'], pointId = pointInterval) %>% 
  #       as.data.frame()
  #     
  #     #### Insert rows in place
  #     trackTable <- rbind(trackTable, newRows)
  #   }
  #   
  #   #### Add current row
  #   trackTable <- rbind(trackTable, pointTable[rowId, ])
  #   
  # }
  # 
  # # Remove dupes created after inserting
  # trackTable %<>% dplyr::distinct()
  # 
  # 
  assign('pointTable', pointTable, envir =  .GlobalEnv)
  # 
  # 
  # 
  # # Output
  # return(trackTable)
  # 
  
}