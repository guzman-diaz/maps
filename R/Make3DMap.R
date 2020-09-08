Make3DMap <- function(mapObject_lst){
  
  # Unlist
  ele_matrix <- mapObject_lst[['ele_matrix']]
  tif_tensor <- mapObject_lst[['tif_tensor']]
  track_lst <- mapObject_lst[['track_lst']]

  
  # Plot
  plot_3d(tif_tensor, ele_matrix, 
          windowsize = c(1100,900), 
          zscale = 50, zoom = 0.65, 
          phi = 45, theta = -45, fov=30, 
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )  
}