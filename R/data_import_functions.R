
#' loads tevi-data
#'
#' @param file_path path as supplied from fileInput
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

  # standardize var-names (remove different prefix for ax-/radial-data)
  names(df) <- map_chr(names(df), ~ str_remove(.x, "^a_|^r_"))

  # check if df contains all obligatory variables
  # add exp-time: time=0 is start of measurement
  if(!(df %>% rlang::has_name("seconds"))){
    stop("Uploaded data has no column with name 'seconds'.
         This variable is needed to construct the experimental-time-variable 't'")}
  # sometimes the pyro-temp and htr_i-variables have slightly different names
  #df <- df %>% rename(pyro_temp = contains("pyro"))
  if(!(df %>%  rlang::has_name("pyro_temp"))){
    stop("Could not detect the variable containing the pyro-temp measurements.
         Make sure that the .csv-file has a variable named 'pyro_temp' holding the Pyrometer-Measurments.")}
  #df <- df %>% rename(htr_i = contains("htr_i"))
  if(!(df %>% rlang::has_name("htr_i"))){
    stop("Could not detect the variable that contains the Heating-Current.
         Make sure that the .csv-file has a variable named 'htr_i' holding the Heater-Current-Values.")
  }

  df <-  df %>% arrange(seconds) %>% mutate(t = seconds - seconds[1])

  df <- df %>% add_temperature()

  df
}




