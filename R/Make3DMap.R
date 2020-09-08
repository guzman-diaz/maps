Make3DMap <- function(mapObject_lst,
                      zscale = 50
                      ){
  
  # Unlist
  ele_matrix <- mapObject_lst[['ele_matrix']]
  ele_raster <- mapObject_lst[['ele_raster']]
  tif_tensor <- mapObject_lst[['tif_tensor']]
  track_lst <- mapObject_lst[['track_lst']]

  
  # Plot
  plot_3d(tif_tensor, ele_matrix, 
          windowsize = c(1100,900), 
          zscale = zscale, zoom = 0.65, 
          phi = 45, theta = -45, fov=30, 
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )  
  
  
  add_gps_to_rayshader(ele_raster, 
                       lat = track_lst[[1]]$lat, 
                       lon = track_lst[[1]]$lon, 
                       alt = track_lst[[1]]$elevation, 
                       zscale = zscale,
                       alpha = 1
  )
}