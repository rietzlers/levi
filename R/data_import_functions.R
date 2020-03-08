
#' loads tevi-data
#'
#' @param file_path
#'
#' @return a tibble with all signals + vars: \emph{t, smoothed_temperature}
#' @export
import_tevi_data <- function(file_path) {
  # load data as is
  df_raw <-
    vroom::vroom(
      file_path, delim = "\t",
      col_types = cols(`Absolute Time` = col_time(format = "%H:%M:%OS"), .default = col_double()),
      trim_ws = TRUE
    )

  validate(need(nrow(df_raw) > 0, "there are no observations in uploaded data"))

  # clean col_names to snake-case
  df_raw <- df_raw %>% janitor::clean_names()

 # delete first row with units
  df <- df_raw[-1, ]

  # add exp-time: time=0 is start of measurement
  df <-  df %>% arrange(seconds) %>% mutate(t = seconds - seconds[1])

  # standardize var-names (remove different prefix for ax-/radial-data)
  names(df) <- map_chr(names(df), ~ str_remove(.x, "^a_|^r_"))

  df %>% add_temperature()

}




