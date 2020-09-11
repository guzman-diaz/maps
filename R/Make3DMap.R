Make3DMap <- function(mapObject_lst,
                      zscale = 50,
                      frames_rate = 30, # frames/second
                      video_seconds = 10,
                      path_laps = 3, # laps/video_seconds
                      camera_laps = 2, # laps/video_seconds
                      theta_ini = -45, # degrees
                      go_animate = F
){
  
  # Unlist
  ele_matrix <- mapObject_lst[['ele_matrix']]
  ele_raster <- mapObject_lst[['ele_raster']]
  tif_tensor <- mapObject_lst[['tif_tensor']]
  track_lst <- mapObject_lst[['track_lst']]
  
  
  # Close previous plot
  if (rgl::rgl.cur() != 0){
    rgl::rgl.close()
  }
  
  
  # Plot 3D
  if (!go_animate){
    
    ## Plot surface
    plot_3d(tif_tensor, ele_matrix,
            windowsize = c(1100,900),
            zscale = zscale, zoom = 0.65,
            phi = 45, theta = theta_ini, fov = 30,
            background = "#F2E1D0", shadowcolor = "#523E2B"
    )
  
  
    ## Overlay tracks
    lapply(track_lst, function(x){
      ## Tranform track CRS to UTM
      x[, 1:2] <- TransformCoordinates(x[, 1:2], is.lonLat = T)
      
      ## Render path
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
  
  # Animate
  if (go_animate){
    
    ## Calculate required number of frames
    frames_num <- video_seconds * frames_rate
    
    lapply(track_lst[1], function(x){ # if a track list is provided, use only the first track
      
      ## Transform track CRS to UTM
      x[, 1:2] <- TransformCoordinates(x[, 1:2], is.lonLat = T)
      
      ## Render frames
      ### Define track segments: each segment from point 1 to 'track_idx[frame_id]'
      track_length <- nrow(x)
      track_idx <- ceiling(seq(from = 1, 
                               to = track_length, 
                               length.out = frames_num / path_laps + 1
      ))[-1] %>% 
        rep(path_laps)
      
      ### Define set of angles that guarantees the required number of camera laps
      view_angle <- seq(0, 360*camera_laps, length.out = frames_num)
      
      ### Loop over all frames
      for (frame_id in 1:frames_num){
        
        #### If lap completed, delete path and start over
        if (track_idx[frame_id] == min(track_idx) & frame_id > 1){
          render_path(clear_previous = T)
        }
        
        #### Render the path segment
        render_path(extent = attr(ele_raster, 'extent'),
                    lat = unlist(x$lat),
                    long = unlist(x$lon),
                    altitude = NULL,
                    zscale = zscale,
                    heightmap = ele_matrix / zscale,
                    color = 'red',
                    offset = 1
        )
        
        #### Render the camera view
        render_camera(theta= theta_ini + view_angle[frame_id])

        #### Save snapshot in disk
        render_snapshot(filename = file.path(here::here('tmp', sprintf('videoFrame%i.png', frame_id))))
      }
    })
    
    ## Encode video
    av::av_encode_video(file.path(here::here('tmp', sprintf('videoFrame%i.png', seq(1, frames_num, by=1)))),
                        framerate = frames_rate,
                        output =here::here('output', 'animation.mp4')
    )
    
    ## Remove frame files
    dir(here::here('tmp'), pattern = 'videoFrame*', full.names = T) %>% 
      file.remove()
  }
  
  }