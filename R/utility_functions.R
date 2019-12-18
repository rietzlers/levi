
#' fit_lorentz
#'
#' @param data tibble with columns f and fc_amp
#' @param sr sample-rate
#' @param c0 vector with start-parameters
#'
#' @return list(fit_params, fitted[[lf_amp]]) NULL if nls did not converge
#' @export
fit_lorentz <- function(data, sr = 400, c0 = c(A = 1000, f0 = 30, g = 0.1) ) {
  tryCatch({
    lfit <-
      nls(
        fc_amp ~ A / ((f ^ 2 - f0 ^ 2) ^ 2 + g ^ 2 * f ^ 2),
        data = data %>% filter(f < sr / 2),
        start =  c0,
        trace = FALSE
      )
    fit_params = as_tibble(summary(lfit)$coeff) %>% janitor::clean_names()
    fitted = data %>%
      filter(f < sr / 2) %>%
      mutate(lf_amp = predict(lfit))
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


#' gen_example data
#'
#' @param sr sample-rate
#' @param dc damping-constant
#' @param f0 signal-freq
#' @param fs freq-shift
#' @param noise_sd sd of additive nois
#' @param wr window-range
#' @param bp band-pass-range
#'
#' @return tibble with vars: t, sig, f, fc, fc_amp, fc_phase, sig_re (reconstructed sig after bp), equal
#' @export
gen_example_data <-
  function(T = 10, sr = 400, dc = 0.1, f0 = 30, fs = 0, noise_sd = 0.1, wr = c(0, 10), bp = c(0, 400)) {
    tibble(
      t = seq(0, T, by = 1 / sr),
      sig = exp(-dc * t) * (sin(2 * pi * (f0 - fs * t) * t)) + rnorm(n = length(t), sd = noise_sd)
    ) %>%
      filter(t %>% between(wr[1], wr[2])) %>%
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


#' get_spectrum
#'
#' @param data tibble with variable sig
#' @param sr sample-rate
#'
#' @return
get_spectrum <- function(data, sr){
  data %$%
    tibble(
      f = spectrum(ts(sig), frequncy = sr, plot = FALSE)$freq * sr,
      fc_amp = spectrum(ts(sig), frequncy = sr, plot = FALSE)$spec
    )
}
