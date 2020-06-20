
#' Estimate Spectrum from Signal
#'
#' @param signal_data tibble with signal
#' @param signal_name char: name of the signal-variable
#' @param frame_rate sample-rate in Hz
#' @param spans vector of odd integers giving the widths of modified Daniell smoothers to be used to smooth the periodogram.
#' @param taper specifies the proportion of data to taper
#'
#' @return tibble with vars: f, spec and fc_amp
#'
#' @export
estimate_signal_spectrum <-
  function(signal_data, signal_name, frame_rate,  spans = c(3, 3), taper = 0.1) {

  est_spec <-
    spectrum(ts(signal_data[[signal_name]], frequency = frame_rate),
             spans = spans, taper = taper,
             demean = TRUE,
             detrend = FALSE, plot = FALSE)

  tibble(
    f = est_spec$freq,
    calc_method = "spectrum",
    spec = est_spec$spec,
    fc_amp = sqrt(spec)
    ) %>%
    dplyr::union(
      levi::fftc(signal_data, signal_name, sr = frame_rate) %>%
        mutate(calc_method = "fft") %>%
        select(f, calc_method, spec, fc_amp) %>%
        dplyr::filter(f < frame_rate/2)
      )
}

# lorentz-fit ----------------

#' fit_lorentz
#'
#' fits a lorentz-curve to the supplied amplitudes of the
#' Fourier-Coefficients. It does not matter if those amplitudes
#' are calculated with the fft or spectrum function.
#'
#' @param fc_data tibble with columns f and fc_amp
#' @param c0 numeric vector c(A, f0, d) with start-values for nls
#' @param nr_tries # to repeat nls with different start-values if
#' it did not converge the first time.
#'
#' @return nls-fit-object or NULL
#' @export
fit_lorentz <- function(fc_data, c0, nr_tries = 10){

  if(missing(c0)){
    c(f, fc_amp) %<-% get_dom_freq(fc_data)
    c0 = c(A = fc_amp^2, f0 = f, d = 0.5)
  }

  fc_data <-
    fc_data %>%
    transmute(
      f,
      fc_amp_squared = fc_amp^2
    )

  fit <- function(fc_data, c0 = c0){
    tryCatch(
      error = function(cnd) {
        #warning(str_glue("nls did not converge;  start_values: ({c0[1]}, {c0[2]}, {c0[3]})"))
        return(NULL)
      },
      # run nls
      {
        lfit <-
          nls(
            fc_amp_squared ~ A / ((f ^ 2 - f0 ^ 2) ^ 2 + (2 * d) ^ 2 * f ^ 2),
            data = fc_data,
            start =  c0,
            trace = FALSE,
            control = list(minFactor = 1 / 1024 ^ 2)
          )
        return(lfit = lfit)
      }
    )
  }

  for(i in 1:nr_tries){
    lf_model <- fit(fc_data, c0 = c0)
    if(!is.null(lf_model)) return(lf_model)
    # else: try with modified start-values
    c0 <- rnorm(n = 3, mean = c0, sd = c0/10)
    names(c0) <- c("A", "f0", "d")
  }
  return(NULL)
}

#' Extract the fitted parameters from fitted lorentz-model;
#' returns NULL-values if lf_model = NULL, i.e. fit did not converge
#'
#' @param lf_model a nls-object as returned from fit_lorentz
#'
#' @return named vector: c("A", "f0", "d")
#' @export
lorentz_parameters <- function(lf_model){
  if(!is.null(lf_model)){
    return(abs(round(coef(lf_model), 2)))
  }else{
    return(c("A" = NA_real_, "f0" = NA_real_, "d" = NA_real_))
  }
}


#' calculate lorentz-amplitudes from fitted model
#'
#' @param freqs a tibble with a variable f
#' @param lf_model a nls model returned from fit_lorentz
#'
#' @return tibble with two variables: f and lf_amp
#'
#' @export
lorentz_amps <- function(freqs, lf_model){
  freqs %>%
    transmute(f, lf_amp = sqrt(predict(lf_model, newdata = list(f = f))))
}
# helper-functions-------------

#' get dominant freq and corresponding amplitude in signal
#'
#' Ermittelt die dominante Frequenz (die Frequenz mit maximaler Amplitude)
#' eines Signals aus dem Periodogramm.
#'
#' Nur Frequenzen ab 0.1 Hz kommen in Frage!
#'
#' Ist das signal ein reines cosinoid, d.h. eine evtl. verrauschte
#' harmonische Schwingung so liefert die Theorie: Der Betrag des betragsmäßig
#' größten (normierten) Fourierkoeffizienten ist die halbe Amplitude des Signals.
#' Aus diesem Grund wird der zurückgegebene Wert der Amplitude mit 2
#' multipliziert.
#'
#' @param fc_data datensatz mit variablen f und fc_amp
#' @param sample_rate sample-rate
#'
#' @return tibble with variables f and fc_amp
#' (fc_amp is the signal-amplitude of the dominant frequency!
#' Not the amplitude of the Fourie-Coefficient!)
#' @export
get_dom_freq <- function(fc_data, sample_rate = 400){
  fc_data %>%
    slice(which.max((fc_amp))) %>%
    dplyr::transmute(f = f, fc_amp = 2 * fc_amp)
}


#' check time-series for regularity
#'
#' checks if sampling-interval is equidistant and no
#' observations are missing
#'
#' @param data tibble with var t
#' @param sr samplerate
#' @param signal character: Name of the signal to check
#'
#' @return boolean: true if ts is regular
#' @export
regular_ts <- function(data, signal, sr) {
  if (any(is.na(data$t))) {
    warning("NA values in t")
    return(FALSE)
  }
  if (any(is.na(data[[signal]]))) {
    warning("NA values in signal")
    return(FALSE)
  }
  if (any(!near(diff(data$t), 1 / sr))) {
    warning("t_i are not equidistant or inconsistent with samplerate")
    return(FALSE)
  }
  return(TRUE)
}
