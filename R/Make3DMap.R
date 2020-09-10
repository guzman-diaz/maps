Make3DMap <- function(mapObject_lst,
                      zscale = 50
){
  
  # Unlist
  ele_matrix <- mapObject_lst[['ele_matrix']]
  ele_raster <- mapObject_lst[['ele_raster']]
  tif_tensor <- mapObject_lst[['tif_tensor']]
  track_lst <- mapObject_lst[['track_lst']]
  
  
  # Close previous plot
  rgl::rgl.close()
  
  
  # Plot surface
  plot_3d(tif_tensor, ele_matrix,
          windowsize = c(1100,900),
          zscale = zscale, zoom = 0.65,
          phi = 45, theta = -45, fov=30,
          background = "#F2E1D0", shadowcolor = "#523E2B"
  )
  
  
  # Overlay tracks
  # lapply(track_lst[1], function(x){
  #   ## Tranform track CRS to UTM
  #   x[, 1:2] <- TransformCoordinates(x[, 1:2], is.lonLat = T)
    
    #   ## Render path
    #   render_path(extent = attr(ele_raster, 'extent'), 
    #               lat = unlist(x$lat), 
    #               long = unlist(x$lon), 
    #               altitude = NULL, 
    #               # zscale = zscale,
    #               heightmap = ele_matrix / zscale,
    #               color = 'red',
    #               offset = 1
    #               
    #   )
    # })
    # 
    # Overlay tracks
    lapply(track_lst[1], function(x){
      ## Transform track CRS to UTM
      x[, 1:2] <- TransformCoordinates(x[, 1:2], is.lonLat = T)
      
      ## Render path
      frames_num <-500
      video_seconds <- 10
      frames_rate <- frames_num / video_seconds

      path_laps <- 3
      track_length <- nrow(x)
      track_idx <- ceiling(seq(from = 1, 
                               to = track_length, 
                               length.out = frames_num / path_laps + 1
      ))[-1] %>% 
        rep(path_laps)
      
      view_laps <- 2      
      view_angle <- seq(0, 360*view_laps, length.out = frames_num)
      
      for (frame_id in 1:frames_num){
        
        #### Select track indices of the frame path segment

        if (track_idx[frame_id] == min(track_idx) & frame_id > 1){
          render_path(clear_previous = T)
        }
        
        #### Render the path segment
        render_path(extent = attr(ele_raster, 'extent'), 
                    lat = unlist(x[1:track_idx[frame_id], 'lat']), 
                    long = unlist(x[1:track_idx[frame_id], 'lon']), 
                    altitude = NULL, 
                    heightmap = ele_matrix / zscale,
                    color = 'red',
                    offset = 1
        )
        
        #### Render the point of view
        render_camera(theta= -45 + view_angle[frame_id])

        render_snapshot(filename = file.path(here::here('tmp', sprintf('videoFrame%i.png', frame_id))))
      }
    })
    
    ## Encode video
    av::av_encode_video(file.path(here::here('tmp', sprintf('videoFrame%i.png', seq(1, frames_num, by=1)))),
                        framerate = frames_rate,
                        output = "animation.mp4"
    )
    
    
    # angles<- seq(0, 360, length.out = 1441)[-1]
    # for(i in 1:1440) {
    #   render_camera(theta = -45+angles[i])
    # }
    # rgl::rgl.close()
  }