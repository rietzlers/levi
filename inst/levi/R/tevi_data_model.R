## setup_tevi_data.R ##

import_tevi_data <- function(session, file) {

  validate(need(file, "upload a tevi-data-file (.dat-file)"))

  # load data as is
  df_raw <-
    vroom::vroom(
      file$datapath, delim = "\t",
      col_types = cols(`Absolute Time` = col_time(format = "%H:%M:%OS"), .default = col_double()),
      trim_ws = TRUE
    )

  validate(need(nrow(df_raw) > 0, "there are no observations in uploaded data"))

  # clean col_names to snake-case
  df_raw <- df_raw %>% janitor::clean_names()

 # delete first row with units
  df <- df_raw[-1, ]

  # add exp-time: time=0 is start of measurement
  df <-  df %>% arrange(seconds) %>% mutate(time = seconds - seconds[1])

  # standardize var-names (remove different prefix for ax-/radial-data)
  names(df) <- map_chr(names(df), ~ str_remove(.x, "^a_|^r_"))

  df

}

to_surface_tension <- function(freq, mass){
  3/8 * pi * mass / 1000  * freq^2
}

to_temperature <- function(data, time){
  # temperatur sollte im interessierenden bereich ! monoton fallend sein (bis auf heizpulse)!
  # stark verrauschte signale sollten geglättet werden.
  approx(data$time, data$pyro_temp, time)$y
}

estimate_signal_spectrum <- function(data, signal, frame_rate) {
  est_spec <-
    spectrum(ts(data  %>% pull(!!signal), frequency = frame_rate),
             plot = FALSE)

  tibble(freq = est_spec$freq, spec = log(est_spec$spec))
}
