#modification of ca_animate function developed by fdschneider
#https://github.com/fdschneider/caspr/blob/master/R/animate.R

ca_animate_threeplots <- function(x,y,z, filename, type = "gif",  speed = 1, directory = getwd() , ...) {
  
  #get root of filename
  
  filename <- sub(".avi|.gif|.mpg|.mp4", "", filename)
  
  if(Sys.info()[['sysname']] == "Linux") X11.options(antialias = "none") #for Linux Systems to enable pixel-wise plotting in (animated) gif-files. 
  if(Sys.info()[['sysname']] == "Windows") windows.options(antialias = "none") #for Windows Systems to enable pixel-wise plotting in (animated) gif-files. 
  
  if("gif" %in% type) {
    
    animation::saveGIF( 
      for(i in 1:length(x$landscapes) ) {
        par(mfrow=c(1,3),mar = c(0,0,0,0))
        plot(x$landscapes[[i]], cols = x$model$cols, grid = FALSE, ani = TRUE)
        plot(y$landscapes[[i]], cols = y$model$cols, grid = FALSE, ani = TRUE)
        plot(z$landscapes[[i]], cols = x$model$cols, grid = FALSE, ani = TRUE)
        }, 
      movie.name = paste0(filename, ".gif"), 
      img.name = "landscape", 
      #convert = "convert", 
      interval = 0.1/speed,
      clean = FALSE, 
      ani.width = x$landscapes[[1]]$dim[1], 
      ani.height = x$landscapes[[1]]$dim[2], 
      outdir = directory,
      ...
    )
  } 
  if("mp4" %in% type) {
    animation::saveVideo( 
      for(i in 1:length(x$landscapes) ) {
        par(mfrow=c(1,3),mar = c(0,0,0,0))
        plot(x$landscapes[[i]], cols = x$model$cols, grid = FALSE, ani = TRUE)
        plot(y$landscapes[[i]], cols = y$model$cols, grid = FALSE, ani = TRUE)
        plot(z$landscapes[[i]], cols = z$model$cols, grid = FALSE, ani = TRUE)
      }, 
      video.name = paste0(filename, ".mp4"), 
      img.name = "landscape", 
      convert = "convert", 
      interval = 0.1/speed,
      cmd.fun = system, 
      clean = TRUE, 
      ani.width = x$landscapes[[1]]$dim[1]*5*3, 
      ani.height = x$landscapes[[1]]$dim[2]*5, 
      outdir = directory,
      ...
    )
  }
  if("avi" %in% type) {
    animation::saveVideo( 
      for(i in 1:length(x$landscapes) ) {
        par(mfrow=c(1,3),mar = c(0,0,0,0))
        plot(x$landscapes[[i]], cols = x$model$cols, grid = FALSE, ani = TRUE)
        plot(y$landscapes[[i]], cols = y$model$cols, grid = FALSE, ani = TRUE)
        plot(z$landscapes[[i]], cols = z$model$cols, grid = FALSE, ani = TRUE)
        }, 
      video.name = paste0(filename, ".avi"), 
      img.name = "landscape", 
      convert = "convert", 
      interval = 0.1/speed,
      cmd.fun = system, 
      clean = TRUE, 
      ani.width = x$landscapes[[1]]$dim[1]*5*3, 
      ani.height = x$landscapes[[1]]$dim[2]*5, 
      outdir = directory,
      ...
    )
  }
  if("wmv" %in% type) {
    animation::saveVideo( 
      for(i in 1:length(x$landscapes) ) {
        par(mfrow=c(1,3),mar = c(0,0,0,0))
        plot(x$landscapes[[i]], cols = x$model$cols, grid = FALSE, ani = TRUE)
        plot(y$landscapes[[i]], cols = y$model$cols, grid = FALSE, ani = TRUE)
        plot(z$landscapes[[i]], cols = z$model$cols, grid = FALSE, ani = TRUE)
      }
      , 
      video.name = paste0(filename, ".wmv"), 
      img.name = "landscape", 
      convert = "convert", 
      interval = 0.1/speed,
      cmd.fun = system, 
      clean = TRUE, 
      ani.width = x$landscapes[[1]]$dim[1]*5*3, 
      ani.height = x$landscapes[[1]]$dim[2]*5, 
      outdir = directory,
      ...
    )
  }
}
