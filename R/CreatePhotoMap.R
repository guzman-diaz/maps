CreatePhotoMap <- function(folderName_data = '\\\\pocpaco\\photo\\maps\\data\\', # here::here('data', 'photos'),
                           folderName_output = '\\\\pocpaco\\photo\\maps\\output\\', # here::here('output'),
                           go_copyPhotos = FALSE,
                           mapStyle = 'OpenTopoMap' # http://leaflet-extras.github.io/leaflet-providers/preview/
){
  
  # Preliminaries
  pacman::p_load(exiftoolr)
  # install_exiftool()
  pacman::p_load(leaflet)
  pacman::p_load(mapview)
  pacman::p_load(htmlwidgets)
  
  # Get exif data
  ## jpg file in input folder, with full path
  fileNames <- paste(folderName_data,
                     list.files(folderName_data, pattern = '*.jpg', recursive = T),
                     sep = '/'
  )
  
  cat(sprintf('%2d files found\n', length(fileNames)))
  
  ## Extract exif data
  exifData <- exiftoolr::exif_read(fileNames) %>% 
    ## Fix error with folder name inside exif data
    dplyr::mutate(SourceFile = fileNames)
  
  # Draw map
  outputMap <- leaflet(exifData) %>%
    addProviderTiles(mapStyle) %>% # http://leaflet-extras.github.io/leaflet-providers/preview/
    addCircleMarkers(fillOpacity = 0.8, radius = 5,
                     lng = ~ exifData$GPSLongitude, lat = ~ exifData$GPSLatitude, 
                     popup = leafpop::popupImage(as.character(exifData$SourceFile))
    )
  
  print(outputMap)
  
  # Export map to html
  folderName_graphs = paste0(folderName_output, '/graphs') # Package mapview requires the photos side-by-side to the widget folder
  folderName_widget = paste0(folderName_output, '/mapWidget')
  
  dir.create(folderName_widget)
  htmlwidgets::saveWidget(outputMap, file = paste0(folderName_widget, '/map.html'), selfcontained = F)

  if (go_copyPhotos){  
    dir.create(folderName_graphs)
    file.copy(fileNames, folderName_graphs, overwrite = T)
  }
  
  invisible(exifData)

}