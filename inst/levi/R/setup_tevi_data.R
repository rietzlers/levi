## setup_tevi_data.R ##

import_tevi_data <- function(session, file) {

  validate(need(file, "upload a tevi-data-file (.dat-file)"))

  # load data as is
  df_raw <-
    vroom::vroom(
      file$datapath,
      delim = "\t",
      col_types = cols(
        `Absolute Time` = col_time(format = "%H:%M:%OS"),
        .default = col_double()
      ),
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

  df

}
