GetMapImage <- function(boundingBox,
                        majorDim = 600,
                        map_type = c('World_Topo_Maps', 'World_Street_Map', 'World_Imagery')[1],
                        imageFileName = here::here('figs', 'overlay.png')
){
  
  GetArcGISMapImage <- function(boundingBox, map_type = "World_Street_Map", file = NULL, 
                                width = 400, height = 400, sr_bbox = 4326) {
    require(httr)
    require(glue) 
    require(jsonlite)
    
    url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
    
    # define JSON query parameter
    web_map_param <- list(
      baseMap = list(
        baseMapLayers = list(
          list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                          map_type = map_type)))
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
  
  ## Define image width
  aspectRatio <- abs((boundingBox$p1$lon - boundingBox$p2$lon) / (boundingBox$p1$lat - boundingBox$p2$lat))
  imageWidth <- ifelse(aspectRatio > 1, majorDim, majorDim*aspectRatio) %>% round()
  imageHeigth <- ifelse(aspectRatio < 1, majorDim, majorDim/aspectRatio) %>% round()
  
  ## fetch overlay image
  imageFileName <- here::here('figs', 'overlay.png')
  GetArcGISMapImage(boundingBox, 
                    map_type = 'World_Imagery', file = imageFileName,
                    width = imageWidth, height = imageHeigth, 
                    sr_bbox = 4326
  )
  
  return(png::readPNG(imageFileName))
  
  
}