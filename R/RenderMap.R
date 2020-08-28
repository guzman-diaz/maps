RenderMap <- function(mapTensorList,
                      ambientLayer_lightness = 0.5,
                      shadeLayer_lightness = 0.7
                      ){
  
  # Unlist tensors
  tif_tensor <- mapTensorList[['tif_tensor']]
  ele_matrix <- mapTensorList[['ele_matrix']]
  
  # Plot
  ambient_layer = rayshader::ambient_shade(ele_matrix, zscale = 10, multicore = TRUE, maxsearch = 200)
  ray_layer = rayshader::ray_shade(ele_matrix, zscale = 20, multicore = TRUE)
  
  tif_tensor %>%
    add_shadow(ray_layer,0.3) %>%
    add_shadow(ambient_layer,0) %>%
    plot_3d(ele_matrix, 
            zscale = 200,
            fov = 0, 
            theta = 15, 
            zoom = 0.6, 
            phi = 60, 
            windowsize = c(1000, 1000),
            solid = FALSE,
            shadow = TRUE,
            shadowdepth = -1
    )
}