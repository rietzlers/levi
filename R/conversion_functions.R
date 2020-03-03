# conversion-functions.R


#' convert frequency to surface tension
#'
#' @param freq dominant frequency of oscillating drop in Hz
#' @param mass Mass of drop in g
#'
#' @return surface tension of liquid alloy in  Nm
#' @export
to_surface_tension <- function(freq, mass){
  3/8 * pi * mass / 1000  * freq^2
}

#' convert damping-constant to alloy-viscosity
#'
#' @param d damping-constant in Hz
#' @param m Mass in g
#' @param r Radius in mm
#'
#' @return alloy-viscosity in mPa * s
#' @export
to_viscosity <- function(d, m, r){
  3/(20*pi) * m/r * d
}

#' convert time to temperature
#'
#' @param signal_data tibble with variable t and pyro_temp (measured temperature)
#' @param interpolation_times times at for which the temperatures are required
#'
#' @return tibble: signal_data with additional variable \emph{smoothed_temp}
#' @export
add_temperature <- function(signal_data, interpolation_times){
  # temperatur sollte im interessierenden bereich ! monoton fallend sein (bis auf heizpulse)!
  # stark verrauschte signale sollten geglättet werden.
  if(missing(interpolation_times)){
    interpolation_times <- signal_data %>% pull(t)
  }

  signal_data %>%
    mutate(
      smoothed_temp = predict(loess(pyro_temp ~ t), newdata = interpolation_times)
    )
}
