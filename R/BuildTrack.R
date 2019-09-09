BuildTrack <- function(pointTable,
                       trackList = NULL
){
  
  # Remove dupes due to map and marker simultaneous clicks
  pointTable %<>% dplyr::distinct(lon, lat, .keep_all = T)
  
  # Format
  pointTable %<>% dplyr::mutate_at(.vars = c(1, 2), .funs = as.character) %>% 
    dplyr::mutate_at(.vars = c(1, 2), .funs = as.numeric)
  
  # Insert in-between pieces of original tracks

  ## Separate point and track ids in new columns
  pointTable$trackId <- substr(pointTable$id, 1, 3)
  pointTable$pointId <- substring(pointTable$id, 5) %>% as.numeric()
  pointTable$id <- NULL

  ## First row
  trackTable <- pointTable[1, ]
  
  ## Insert points
  for (rowId in 2:nrow(pointTable)){
    ### When two consecutive points have the same trackId
    if (pointTable[rowId, 'trackId'] != 'map' & 
        pointTable[rowId, 'trackId'] == pointTable[(rowId-1), 'trackId']
    ){
      #### Get chunk of records from original track
      pointInterval <- pointTable[rowId-1, 'pointId']:pointTable[rowId, 'pointId']
      newRows <- trackList[[as.numeric(pointTable[rowId, 'trackId'])]]$table[pointInterval, ] %>% 
        dplyr::mutate(trackId = pointTable[rowId, 'trackId'], pointId = pointInterval) %>% 
        as.data.frame()
      
      #### Insert rows in place
      trackTable <- rbind(trackTable, newRows)
    }
    
    #### Add current row
    trackTable <- rbind(trackTable, pointTable[rowId, ])
    
  }
  
  # Remove dupes created after inserting
  trackTable %<>% dplyr::distinct()
  
  # Output
  return(trackTable)

}