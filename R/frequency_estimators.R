#' calculate fourier-koeffs. with fft
#'
#' The function \code{fft} returns the \bold{unnormalized} Fourier-Coefficients
#' for the Fourier-Frequencies 0/N, ... (N-1)/N, where N = T/dT ist the Number of samples.
#'
#'
#' @param data tibble with var "signal"
#' @param signal character: name of signal
#' @param sr samplerate
#'
#' @return tibble with vars f (Kreisfrequenz, d.h. cycles per unit-time), fc, fc_amp and fc_arg
#'
#' @export
fftc <- function(data, signal, sr){
  if(!regular_ts(data, signal, sr)) stop("no regular ts")

  N <- length(data[[signal]])

  data %>%
    transmute(
      fc = fft(.data[[signal]]) / N, # Normalisierung !!!
      f = 0:(N - 1) / N * sr,
      fc_amp = Mod(fc),
      fc_arg = Arg(fc)
    )
}


#' fit_lorentz
#'
#' @param fc_data tibble with columns f and fc_amp
#' @param c0 numeric vector c(A, f0, d) with start-values for nls
#' @param sr samplerate
#' @param nr_tries # to repeat nls with different start-values if
#' it did not converge
#'
#' @return list with elements (all element will be NULL if non-convergence):
#' \itemize{
#' \item est_params: c(A, f0, d)
#' \item fitted: tibble with vars: f, lf_amp
#' \item lfit: model-object
#' \item start_values:
#' }
#' @export
fit_lorentz <- function(fc_data, c0, sr, nr_tries = 10){

  if(missing(c0)){
    c(f, fc_amp) %<-% get_dom_freq(fc_data, sample_rate = sr)
    c0 = c(A = fc_amp^2, f0 = f, d = 0.5)
  }

  fit <- function(fc_data, c0 = c0, sr = sr)
  {
    tryCatch(
      error = function(cnd) {
        return(list(
          est_params = NULL,
          fitted = NULL,
          lfit = NULL,
          start_values = c0
        ))
      },
      # run nls
      {
        lfit <-
          nls(
            fc_squared ~ A / ((f ^ 2 - f0 ^ 2) ^ 2 + (2 * d) ^ 2 * f ^ 2),
            data = fc_data %>% mutate(fc_squared = fc_amp ^ 2),
            start =  c0,
            trace = FALSE,
            control = list(minFactor = 1 / 1024 ^ 2)
          )

        fit_params <-
          as_tibble(summary(lfit)$coeff) %>% janitor::clean_names()

        fitted <-
          fc_data %>%
          mutate(lf_amp = sqrt(predict(lfit, newdata = f)))

        return(list(
          est_params = fit_params,
          fitted = fitted,
          lfit = lfit,
          start_values = c0
        ))
      }
    )
  }

  result <- fit(fc_data, c0 = c0, sr = sr)
  if(!is.null(result$lorentz_fit)) return(result)

  for(i in 1:nr_tries){
    c0 <- rnorm(n = 3, mean = c0, sd = c0/10)
    names(c0) <- c("A", "f0", "d")
    #message(str_glue("\n start_values: ({c0[1]}, {c0[2]}, {c0[3]})"))
    result <- fit(fc_data, c0 = c0, sr = sr)
    if(!is.null(result$lorentz_fit)) {
      return(result)
      }
    }
  return(result)
}

#' Band-Pass-Filter signal
#'
#' @param sig_data tibble with var signal
#' @param signal character: name of signal to be filtered
#' @param bp numeric vector: lower and upper bp-Freqs
#' @param sr samplerate
#'
#' @return tibble with vars t and signal_name
#' @export
bp_filter <- function(sig_data, signal_name, bp, sr){
  N <- length(sig_data$t)
  levi::fftc(sig_data, signal_name, sr) %>%
    mutate(
      t = sig_data$t,
      fc = if_else(
        between(f, bp[1], bp[2]) | between(f, (sr - bp[2]), (sr - bp[1])),
        fc * N, # N = lenght(t): Normierung wieder rückgängig machen.
        0i
        ),
      !!sym(signal_name) := Re(fft(fc, inverse = TRUE)) / N # Normierung!
    )
}

#' get dominant freq and corresponding amplitude in signal
#'
#' ermittle die dominante Frequenz eines signals aus dem Periodogramm
#' Ist das signal ein reines cosinoid, d.h. eine evtl. verrauschte
#' harmonische Schwingung so liefert die Theorie: Der Betrag des betragsmäßig
#' größten Foruierkoeffizienten ist die halbe Amplitude des Signals.
#' Aus diesem Grund wird der zurückgegebene Wert der Amplitude mit 2
#' multipliziert.
#'
#' @param fc_data datensatz mit variablen f und fc_amp
#' @param sample_rate sample-rate
#'
#' @return tibble with variables f and fc_amp
#' @export
get_dom_freq <- function(fc_data, sample_rate = 400){
  fc_data %>%
    dplyr::filter(f < sample_rate/2) %>% # spiegelsymmetrie an der nyquist-freq!
    dplyr::filter(near(fc_amp, max(fc_amp)))  %>%
    dplyr::transmute(f = f, fc_amp = 2 * fc_amp)
}


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
