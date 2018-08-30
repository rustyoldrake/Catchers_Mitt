# https://github.com/thomasp85/ambient/blob/master/README.md

# install.packages('devtools')
#  devtools::install_github('thomasp85/ambient')
# # video here https://www.youtube.com/watch?v=OnN-AbavAW4

library(ambient)

plot_raster <- function(mat) {
  mat <- (mat - min(mat)) / diff(range(mat))
  plot(as.raster(mat))
}

# Simplex
plot_raster(noise_simplex(c(500, 500)))

# Simplex - No fractality
plot_raster(noise_simplex(c(500, 500), fractal = 'none'))

# Worley - with pertubation
plot_raster(noise_worley(c(500, 500), pertubation = 'normal', pertubation_amplitude = 40))
plot_raster(noise_worley(c(500, 500), pertubation = 'normal', pertubation_amplitude = 4))
plot_raster(noise_worley(c(500, 500), pertubation = 'normal', pertubation_amplitude = 200))


while(1) {
 plot_raster(noise_worley(c(1000, 1000), pertubation = 'normal', pertubation_amplitude = sample(1:200, 1)))
  Sys.sleep(.5)
}
