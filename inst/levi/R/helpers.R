## helpers.R ##


select_data <- function(session, data, brush_data) {
  selected_data <-
    brushedPoints(data, brush_data, xvar = "time")
  validate(need(
    nrow(selected_data) > 0,
    "select data by brushing (left-click and pull) over signal-plot"
  ))
  selected_data
}

estimate_signal_spectrum <- function(session, data, signal, frame_rate) {
  est_spec <-
    spectrum(ts(data  %>% pull(!!signal), frequency = frame_rate),
             plot = FALSE)

  tibble(freq = est_spec$freq, spec = log(est_spec$spec))
}
