GetMapImage <- function(boundingBox,
                        mapType = 'esri-topo',
                        imageWidth, 
                        imageHeight,
                        imageFileName = here::here('figs', 'overlay.png'),
                        heightMagnification = 1.07
){
  
  pacman::p_load(OpenStreetMap)
  
  # Define function to import from ArcGis
  GetArcGISMapImage <- function(boundingBox, mapType = "World_Street_Map", file = NULL, 
                                width, height, sr_bbox = 4326) {
    require(httr)
    require(glue) 
    require(jsonlite)
    
    url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
    
    # define JSON query parameter
    web_map_param <- list(
      baseMap = list(
        baseMapLayers = list(
          list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{mapType}/MapServer",
                                          mapType = mapType)))
        )
      ),
      exportOptions = list(
        outputSize = c(width, height)
      ),
      mapOptions = list(
        extent = list(
          spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
          xmax = jsonlite::unbox(max(boundingBox$p1$lon, boundingBox$p2$lon)),
          xmin = jsonlite::unbox(min(boundingBox$p1$lon, boundingBox$p2$lon)),
          ymax = jsonlite::unbox(max(boundingBox$p1$lat, boundingBox$p2$lat)),
          ymin = jsonlite::unbox(min(boundingBox$p1$lat, boundingBox$p2$lat))
        )
      )
    )
    
    res <- GET(
      url, 
      query = list(
        f = "json",
        Format = "PNG32",
        Layout_Template = "MAP_ONLY",
        Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
    )
    
    if (status_code(res) == 200) {
      body <- content(res, type = "application/json")
      message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
      if (is.null(file)) 
        file <- tempfile("overlay_img", fileext = ".png")
      
      img_res <- GET(body$results[[1]]$value$url)
      img_bin <- content(img_res, "raw")
      writeBin(img_bin, file)
      message(paste("image saved to file:", file))
    } else {
      message(res)
    }
    invisible(file)
  }
  
  #-------------------------------------------------------------
  
  # Map types
  if (mapType %in% c('osm', 'bing', 'stamen-toner', 
                     'maptoolkit-topo',
                     'waze', 'mapquest', 'mapquest-aerial',
                     'stamen-terrain',
                     'stamen-watercolor', 'osm-german', 'osm-wanderreitkarte',
                     'mapbox', 'esri', 'esri-topo',
                     'nps', 'apple-iphoto', 'skobbler',
                     'opencyclemap', 'osm-transport',
                     'osm-public-transport', 'osm-bbike', 'osm-bbike-german'
  )){
    
    ## Download OSM
    mapObject <- openmap(upperLeft = c(lat = boundingBox$p2$lat, lon = boundingBox$p1$lon),
                         lowerRight = c(lat = boundingBox$p1$lat, lon = boundingBox$p2$lon), 
                         zoom = NULL, 
                         type = mapType,
                         mergeTiles = TRUE
    )
    
    ## Fetch overlay image
    png(filename = imageFileName, width = imageWidth, height = imageHeight * heightMagnification)
    plot(mapObject)
    dev.off()
    
  } else {
    ## Download ArcGIS
    mapObject <- GetArcGISMapImage(boundingBox, 
                                   mapType = mapType, 
                                   file = imageFileName,
                                   width = imageWidth, height = imageHeight, 
                                   sr_bbox = 4326
    )
  }
  
  
  return(png::readPNG(imageFileName))
  
}

