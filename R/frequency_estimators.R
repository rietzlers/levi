
#' Berechnung des Stichprobenspektrums
#'
#' periodogram berechnet das Stichprobenspektrum  (Schlittgen Seite 68) einer gegebenen Zeitreihe:
#' \deqn{I(\lambda) = N\cdot |D(\lambda)|^2}
#' \eqn{D(\lambda)}: Fouriertransformierte der Zeitreihe.
#'
#' @param sr
#' @param ts
#'
#' @return
#' @export
periodogram <- function(ts, sr, type){
# check for missing observations!!
}

#' fit_lorentz
#'
#' @param data tibble with columns f and fc_amp
#' @param sr sample-rate
#' @param c0 vector with start-parameters
#'
#' @return list(fit_params, fitted[[lf_amp]]) NULL if nls did not converge
#' @export
fit_lorentz <- function(data, sr = 400, c0 = c(A = 1000, f0 = 30, g = 0.1) ) {
  # falls nicht konvergiert: ändere Startwerte und versuche erneut. ca. 20 mal.
  tryCatch({
    lfit <-
      nls(
        fc_amp_squared ~ A / ((f ^ 2 - f0 ^ 2) ^ 2 + (g / 2) ^ 2 * f ^ 2),
        data = data %>% filter(f < sr / 2) %>% mutate(fc_amp_squared = fc_amp^2),
        start =  c0,
        trace = FALSE
      )
    fit_params = as_tibble(summary(lfit)$coeff) %>% janitor::clean_names()
    fitted = data %>%
      filter(f < sr / 2) %>%
      mutate(lf_amp = sqrt(predict(lfit)))
    list(
      fit_params = fit_params,
      fitted = fitted
    )
  },
  warning = function(cond) {
    return(list(fit_params = NULL, fitted = NULL))
  },
  error = function(cond) {
    return(list(fit_params = NULL, fitted = NULL))
  })
}


#' get_spectrum
#'
#' @param data tibble with variable sig
#' @param sr sample-rate
#'
#' @return
#' @export
get_spectrum <- function(data, sr, type = "spec"){
  if (type == "spec")
  {
    data %$%
      tibble(
        f = spectrum(ts(sig), frequncy = sr, plot = FALSE)$freq * sr,
        fc_amp = spectrum(ts(sig), frequncy = sr, plot = FALSE)$spec
      )
  }else{
    data %>%
      mutate(
        fc = fft(sig),
        f = seq(0, length(t) - 1) / length(t) * sr,
        fc_amp = Mod(fc),
        fc_phase = Arg(fc)
      ) %>%
      mutate(
        bp = if_else(bp[1] <= f & f <= bp[2], fc, 0i),
        sig_re = Re(fft(bp, inverse = TRUE)) / length(t),
        equal = factor(if_else(near(sig, sig_re), "yes", "no"), levels = c("yes", "no"))
      )
  }
}

#' ermittle die dominante Frequenz eines signals aus dem Periodogramm
#'
#'
#' @param data datensatz mit variablen f und fc_amp
#' @param sample_rate sample-rate
#'
#' @return tibble with variables f and fc_amp mit einer observation
#' @export
max_freq <- function(data, sample_rate = 400){
  data %>%
    filter(near(fc_amp, max(fc_amp)))  %>%
    filter(f < sample_rate/2) %>% # spiegelsymmetrie an der nyquist-freq!
    transmute(f = mean(f), fc_amp = mean(fc_amp))
}
