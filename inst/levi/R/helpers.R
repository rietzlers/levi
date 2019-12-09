## helpers.R ##

import_tevi_data <- function(session, file) {
  validate(need(file, "upload a tevi-data-file (.dat-file)"))


  df_raw <-
    vroom::vroom(
      file$datapath,
      delim = "\t",
      col_types = cols(
        `Absolute Time` = col_time(format = "%H:%M:%OS"),
        .default = col_double()
      ),
      trim_ws = TRUE
    ) %>%
    janitor::clean_names()


  df <-
    df_raw[-1, ] %>%
    arrange(seconds) %>%
    mutate(time = seconds - seconds[1])

  names(df) <-
    map_chr(names(df), function(name) {
      str_remove(name, "^a_|^r_")
    })

  validate(need(nrow(df) > 0, "there are no observations in uploaded data"))

  # update UI -
  # available signals
  col_names <- df %>% names()
  signal_names <- col_names[col_names != "time"]
  updateSelectInput(
    session,
    "signal_choice",
    choices = signal_names,
    selected = sample(signal_names, size = 1)
  )
  # estimate sample/frame-rate from mean dt
  c(est_sample_freq) %<-% (df %>% summarize(est_sample_freq = round(1 / mean(
    diff(time), na.rm = TRUE
  ))))
  updateNumericInput(session, "frame_rate", value = est_sample_freq)

  df <-
    df %>%
    mutate(htr_i_norm = htr_i / max(htr_i, na.rm = TRUE))

  df

}

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
