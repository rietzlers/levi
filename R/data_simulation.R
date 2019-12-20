#' Simuliere Signal-daten
#'
#' run the example to see the intended workflow:
#' \enumerate{
#'   \item calc fourier-coefficients
#'   \item get dominant freq. from fft or periodogram
#' }
#'
#' @param T frame size
#' @param sr sample-rate
#' @param noise_sd sd of additive nois
#' @param signal string
#'
#' @return tibble with vars: t, s
#'
#' @examples
#' library(tidyverse)
#' library(magrittr)
#' library(cowplot)
#' library(zeallot)
#'
#' sr <- 100
#' ex_data <- gen_example_data(T = 1, sr = sr, noise_sd = 0)
#'
#'   ex_data # tibble with columns t and s
#' sig_plot <- ex_data %>% ggplot(aes(x = t, y = s)) + geom_line()
#'
# calculate fc
#' sig_fc <- levi::get_fc(ex_data, "s", sr)
#' fcp <- sig_fc %>% ggplot(aes(x = f)) + xlim(c(0, sr / 2))

# bp filter
#' bp_signal <- levi::bp_filter(ex_data, "s", bp = c(30, 40), sr)
#' sig_plot <-
#'   sig_plot +
#'   geom_line(data = bp_signal, aes(x = t, y = s), color = "red")
#'
# max amplitude
#' c(f_max, fmax_amp) %<-% max_freq(sig_fc, sr)
#' fcp <-
#'   fcp + labs(subtitle = str_glue("Dom. Freq. {round(f_max, 2)} Hz / Amp: {round(fmax_amp, 1)}"))

# visualize
#' fcp <-
#'   plot_grid(fcp + geom_line(aes(y = fc_amp)), fcp + geom_line(aes(y = fc_arg)))
#' plot_grid(sig_plot, fcp, nrow = 2)
#' @export
gen_example_data <-
  function(T = 10, sr = 400, noise_sd = 0.1 , signal = "100 * exp(-3 * t) * sin(2*pi*30*t)") {
    tibble(
      t = seq(0, T, by = 1 / sr),
      s = !!rlang::parse_expr(signal) + rnorm(n = length(t), sd = noise_sd)
    )
  }
