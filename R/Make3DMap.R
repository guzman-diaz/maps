Make3DMap <- function(mapObject_lst,
                      zscale = 50
                      ){
  
  # Unlist
  ele_matrix <- mapObject_lst[['ele_matrix']]
  ele_raster <- mapObject_lst[['ele_raster']]
  tif_tensor <- mapObject_lst[['tif_tensor']]
  track_lst <- mapObject_lst[['track_lst']]

  
  # Plot surface
  plot_3d(tif_tensor, ele_matrix,
          windowsize = c(1100,900),
          zscale = zscale, zoom = 0.65,
          phi = 45, theta = -45, fov=30,
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )

  
  # Overlay tracks
  lapply(track_lst, function(x){
    ## Tranform track CRS to UTM
    x[, 1:2] <- TransformCoordinates(x[, 1:2], is.lonLat = T)
 
    render_path(extent = attr(ele_raster, 'extent'), 
                lat = unlist(x$lat), 
                long = unlist(x$lon), 
                altitude = NULL, 
                # zscale = zscale,
                heightmap = ele_matrix / zscale,
                color = 'red',
                offset = 1
                
    )
  })
}