SaveTrack <- function(track_name, 
                      track_proposed = NULL,
                      track_folder = here::here('output'),
                      track_type = '.kmz'
){
  
  # Get proposed track from global environment
  if (is.null(track_proposed)){
    cat('Using proposed track in the Global environment\n')
    track_proposed <- parent.frame()$track_proposed
  }
  
  # Build name
  track_name <- paste0(track_name, track_type)
  
  # Save
  track_proposed %>% 
    dplyr::select(lon, lat) %>% 
    dplyr::mutate(id = track_name, p_id = 1:nrow(.), time = Sys.Date()) %>% 
    dplyr::mutate(track_name = track_name, 
                  file_name = file.path(track_folder, track_name), 
                  track_color = '00ff33ff'
    ) %>% 
    psyosphere::export_kml(t_id = 'id')
}