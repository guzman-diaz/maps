AddTrackToList <- function(trackTable, trackList){
  return(c(trackList, list(list(table = trackTable))))
}