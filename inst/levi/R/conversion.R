## conversion.R ##

to_surface_tension <- function(freq, mass){
  3/8 * pi * mass / 1000  * freq^2
}

to_temperature <- function(data, time){
    approx(data$time, data$pyro_temp, time)$y
}
