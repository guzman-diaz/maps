Make3DMap <- function(mapObjects_lst){
  
  # Unlist
  ele_raster <- mapObject_lst[['ele_raster']]
  tif_raster <- mapObject_lst[['tif_raster']]
  track_lst <- mapObject_lst[['track_lst']]
  
  # Transform elevation to matrix
  ele_matrix <- rayshader::raster_to_matrix(ele_raster)

  ## Remove NAs (ocean)
  ele_matrix[is.na(ele_matrix)] <- 0
  
  
  # Transform TIF
  
  ## Transform each band to array
  tif_tensor_1 <- rayshader::raster_to_matrix(tif_raster[[1]])
  tif_tensor_2 <- rayshader::raster_to_matrix(tif_raster[[2]])
  tif_tensor_3 <- rayshader::raster_to_matrix(tif_raster[[3]])
  
  ## Merge band arrays into one tensor:
  ### Initialize
  tif_tensor <- array(0, dim = c(nrow(tif_tensor_1), ncol(tif_tensor_1), 3))
  
  ### Merge
  tif_tensor[, , 1] <- tif_tensor_1/255 
  tif_tensor[, , 2] <- tif_tensor_2/255 
  tif_tensor[, , 3] <- tif_tensor_3/255
  
  ### Transpose to abide by the elevation raster orientation
  tif_tensor <- aperm(tif_tensor, c(2, 1, 3))

  
  # Plot
  plot_3d(tif_tensor, ele_matrix, 
          windowsize = c(1100,900), 
          zscale = 50, zoom = 0.65, 
          phi = 45, theta = -45, fov=30, 
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )  
  
  # Output
  invisible(list(tif_tensor = tif_tensor, ele_matrix = ele_matrix))
}