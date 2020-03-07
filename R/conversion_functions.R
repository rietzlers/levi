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
#' @param time_range vector: time-limits between which smoothing is done;
#'
#' @return tibble: signal_data with additional variable \emph{smoothed_temp}
#' @export
add_temperature <- function(signal_data, time_range){
  # temperatur sollte im interessierenden bereich ! monoton fallend sein (bis auf heizpulse)!
  # stark verrauschte signale sollten geglättet werden.

  available_times <- range(signal_data %>% pull(t))

  if(missing(time_range)){
    time_range <- available_times
  }

  if(time_range[2] > available_times[2]){
    time_range <- available_times
  }

  if(signal_data %>% has_name("smoothed_temp")){
    signal_data <- signal_data %>% select(-smoothed_temp)
  }


  time_temp_data <-
    signal_data %>%
    filter(
      t %>% between(time_range[1], time_range[2])
    ) %>%
    mutate(
      smoothed_temp = predict(loess(pyro_temp ~ t))
    ) %>%
    select(t, smoothed_temp)

  signal_data %>%
    left_join(time_temp_data, by = c("t"))
}

#' convert time to temp
#'
#' this function takes a time-vector and converts it to a temperature-vector;
#' it uses the smoothed temperature-values from raw_tevi_data:
#' if the time is outside the experimental-time-range (specified in import-tevi-data)
#' it returns NA-values
#'
#' @param time vector: time-points to convert to temp
#' @param tevi_data tibble with vars \emph{t} and \emph{smoothed_temp}
#'
#' @return vector with temperatur
#' @export
convert_to_temp <- function(time, tevi_data){
  approx(x = tevi_data[["t"]], y = tevi_data[["smoothed_temp"]], xout = time)$y %>%
    round(1)
}
