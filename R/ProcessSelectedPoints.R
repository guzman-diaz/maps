ProcessSelectedPoints <- function(pointTable,
                                  trackList = NULL
){
  
  # Remove dupes due to map and marker simultaneous clicks
  pointTable %<>% dplyr::distinct(lon, lat, .keep_all = T)

}