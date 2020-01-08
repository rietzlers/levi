
#' Berechnung des Stichprobenspektrums
#'
#' periodogram berechnet das Stichprobenspektrum  (Schlittgen Seite 68) einer gegebenen Zeitreihe:
#' \deqn{I(\lambda) = N\cdot |D(\lambda)|^2}
#' \eqn{D(\lambda)}: Fouriertransformierte der Zeitreihe.
#'
#' @param sr sample-rate
#' @param ts datensatz mit mind den variablen t und signal
#'
#' @return
#' @export
periodogram <- function(data, sr, signal, type){
  if(!regular_ts(data, sr, signal)) stop("keine reguläre zeitreihe")
}

#' fit_lorentz
#'
#' @param data tibble with columns f and fc_amp
#' @param sr sample-rate
#' @param c0 vector with start-parameters
#' @param signal character
#'
#' @return list(fit_params, fitted[[f, lf_amp]]) NULL if nls did not converge
#' @export
fit_lorentz <- function(data, signal, sr = 400, c0 = c(A = 1000, f0 = 30, g = 0.1) ) {
  # falls nicht konvergiert: ändere Startwerte und versuche erneut. ca. 20 mal.
  fft_data <-
    data %>% get_fc(signal, sr) %>%
    mutate(
      fc_squared = (fc_amp * 2)^2 # siehe schlittgen seite 56
    )
  c(f_dom, fc_amp) %<-% max_freq(fft_data, sample_rate = sr)
  c0 <- c(A = (fc_amp * 2)^2, f0 = f_dom, g = 0.1)

  print(c0)

  # tryCatch({
    lfit <-
      nls(
        fc_squared ~ A / ((f ^ 2 - f0 ^ 2) ^ 2 + (2*g) ^ 2 * f ^ 2),
        data = fft_data %>% filter(between(f, f_dom - 10, f_dom + 10)),
        start =  c0,
        trace = TRUE,
        control = list(minFactor = 1/1024^2)
      )
    fit_params <-
      as_tibble(summary(lfit)$coeff) %>% janitor::clean_names()
    fitted <-
      fft_data %>%
      filter(between(f, f_dom - 10, f_dom + 10)) %>%
      mutate(lf_amp = sqrt(predict(lfit)) / 2)

    return(list(fit_params = fit_params,
                fitted = fitted))
  # },
  # warning = function(cond) {
  #   print("warning: nls of lorentz-fit")
  #   return(list(fit_params = NULL, fitted = NULL))
  # },
  # error = function(cond) {
  #   print("error: nls of lorentz-fit")
  #   return(list(fit_params = NULL, fitted = NULL))
  # })
}


#' get_spectrum
#'
#' @param data tibble with variable sig
#' @param sr sample-rate
#'
#' @return
#' @export
get_spectrum <- function(data, signal, sr){
    data %$%
      tibble(
        f = spectrum(ts(!!sym(signal)), frequncy = sr, plot = FALSE)$freq * sr,
        fc_amp = spectrum(ts(!!sym(signal)), frequncy = sr, plot = FALSE)$spec
      )
}

#' calculate fourier-koeffs. with fft
#'
#' The function \code{fft} returns the \bold{unnormalized} Fourier-Coefficients!
#' To be consistent with Schlittgen and
#'
#' @param data tibble with var "signal"
#' @param signal character: name of signal
#' @param sr samplerate
#'
#' @return tibble with vars f, fc, fc_amp and fc_arg
#'
#' @export
get_fc <- function(data, signal, sr){
  if(!regular_ts(data, signal, sr)) stop("no regular ts")

  signal = sym(signal)
  N <- length(data[[signal]])

  data %>%
    transmute(
      fc = fft(!!signal) / N, # Normalisierung !!!
      f = 0:(N - 1) / N * sr,
      fc_amp = Mod(fc),
      fc_arg = Arg(fc)
    )
}

#' Band-Pass-Filter signal
#'
#' @param sig_data tibble with var signal
#' @param signal character: name of signal to be filtered
#' @param bp numeric vector: lower and upper bp-Freqs
#' @param sr samplerate
#'
#' @return tibble with
#' @export
bp_filter <- function(sig_data, signal, bp, sr){
  N <- length(sig_data$t)
  get_fc(sig_data, signal, sr) %>%
    mutate(
      t = sig_data$t,
      fc = if_else(
        between(f, bp[1], bp[2]) | between(f, (sr - bp[2]), (sr - bp[1])),
        fc * N, # N = lenght(t): Normierung wieder rückgängig machen.
        0i
        ),
      !!sym(signal) := Re(fft(fc, inverse = TRUE)) / N
    )
}

#' ermittle die dominante Frequenz eines signals aus dem Periodogramm
#'
#'
#' @param fft_data datensatz mit variablen f und fc_amp
#' @param sample_rate sample-rate
#'
#' @return tibble with variables f and fc_amp mit einer observation
#' @export
max_freq <- function(fft_data, sample_rate = 400){
  fft_data %>%
    filter(f < sample_rate/2) %>% # spiegelsymmetrie an der nyquist-freq!
    filter(near(fc_amp, max(fc_amp)))  %>%
    transmute(f = mean(f), fc_amp = mean(fc_amp))
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
  if (any(!near(diff(data$t), 1 / sr))) {
    warning("t_i are not equidistant or inconsistent with samplerate")
    return(FALSE)
  }

  if (any(is.na(data[[signal]]))) {
    warning("NA values in signal")
    return(FALSE)
  }

  return(TRUE)
}
