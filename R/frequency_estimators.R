# fourier-transformation ------------

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
#' @return tibble with vars f (Kreisfrequenz, d.h. cycles per unit-time), fc, fc_amp and fc_arg and spec = N * fc_amp^2
#'
#' @export
fftc <- function(data, signal, sr){
  if(!regular_ts(data, signal, sr)) warning("no regular ts")

  N <- length(data[[signal]])

  data %>%
    transmute(
      f = 0:(N - 1) / N * sr,
      fc = fft(.data[[signal]]) / N, # Normalisierung !!!
      fc_amp = Mod(fc),
      fc_arg = Arg(fc),
      spec = N * fc_amp^2
    )
}



#' Band-Pass-Filter signal
#'
#' The function calculates the Fourier-Coefficients with fft,
#' sets all Fourier-Coefficients not within the bp-range to 0
#' and inverses the fft.
#'
#'
#' @param sig_data tibble with var signal
#' @param signal_name var-name of the signal
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
    ) %>%
    select(-fc)
}





#' Estimate Spectrum from Signal
#'
#' @param signal_data tibble with signal
#' @param signal_name char: name of the signal-variable
#' @param frame_rate sample-rate in Hz
#' @param spans vector of odd integers giving the widths of modified Daniell smoothers to be used to smooth the periodogram.
#' @param taper specifies the proportion of data to taper
#'
#' @return tibble with vars: f, fc_amp, type(spectrum/fft), spec
#'
#' @export
estimate_signal_spectrum <- function(signal_data, signal_name, frame_rate,  spans = c(3, 3), taper = 0.1) {

  est_spec <-
    spectrum(ts(signal_data[[signal_name]], frequency = frame_rate),
             spans = spans, taper = taper,
             demean = TRUE,
             detrend = FALSE, plot = FALSE)

  tibble(
    f = est_spec$freq,
    type = "spectrum",
    spec = est_spec$spec,
    fc_amp = sqrt(spec)
    ) %>%
    dplyr::union(
      levi::fftc(signal_data, signal_name, sr = frame_rate) %>%
        mutate(type = "fft") %>%
        select(f, type, spec, fc_amp) %>%
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
#' @param sr samplerate
#' @param nr_tries # to repeat nls with different start-values if
#' it did not converge the first time.
#' @param bp numeric vector of length 2 specifying a Band-Pass; fit to bp-filtered data
#'
#' @return nls-fit-object or NULL
#' @export
fit_lorentz <- function(fc_data, c0, bp, sr, nr_tries = 10){

  if(missing(bp)){
    bp = c(0.1, sr/2)
  }

  if(missing(c0)){
    c(f, fc_amp) %<-% get_dom_freq(fc_data, sample_rate = sr)
    c0 = c(A = fc_amp^2, f0 = f, d = 0.5)
  }

  fc_data <-
    fc_data %>%
    transmute(
      f,
      fc_amp_squared = fc_amp^2
    ) %>%
    filter(
      f %>% between(bp[1], bp[2])
    )

  fit <- function(fc_data, c0 = c0, sr = sr){
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
    lf_model <- fit(fc_data, c0 = c0, sr = sr)
    if(!is.null(lf_model)) return(lf_model)
    # else: try with modified start-values
    c0 <- rnorm(n = 3, mean = c0, sd = c0/10)
    names(c0) <- c("A", "f0", "d")
  }
  return(NULL)
}

#' Extract the fitted parameters from fitted lorentz-model
#'
#' @param lf_model a nls-object as returned from fit_lorentz
#'
#' @return named vector
#' @export
lorentz_parameters <- function(lf_model){
  abs(round(coef(lf_model), 2))
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
    dplyr::filter(f %>% between(0.1, sample_rate/2)) %>%
    dplyr::filter(near(fc_amp, max(fc_amp)))  %>%
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

# plots ------

#' generate spectrogram-plot
#'
#' @param est_spec tibble: estimated spectrum
#' @param lfit lfit-model
#' @param scale string: raw/log10
#' @param bp numeric vector: bandpass-limits
#' @param type_choosen fft/spectrum
#' @param sample_rate sample-rate
#' @param plot_type ggplot/plot_ly
#'
#' @return plot-object
#'
#' @export
gen_spec_plot <-
  function(est_spec, lfit_models, scale = "log10", bp, type_choosen = "fft", sample_rate, plot_type = "ggplot"){

    browser()
    periodogram <-
      est_spec %>%
      filter(f %>% between(bp[1], bp[2])) %>%
      ggplot(aes(x = f)) +
      geom_line(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp)) +
      geom_point(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), shape = "x", size = 0.8) +
      geom_point(data = ~ dplyr::filter(.x, type == "fft", f > 0), aes(y = fc_amp), color = "blue", shape = "+", size = 1.5) +
      scale_y_continuous(
        name = if_else(scale == "log10", "log10(Fourier-Coef-Amp)", "Fourier-Coef-Amp"),
        trans = if_else(scale == "log10", "log10", "identity")
      ) +
      labs(x = "Frequency [Hz]")

    if(!is.null(lfit)){
      fitted_data <-
        tibble(f = seq(bp[1], bp[2], by = 1/100)) %>%
        lorentz_amps(lfit)

      periodogram <-
        periodogram +
        geom_line(data = fitted_data, aes(x = f, y = lf_amp), alpha = 0.5, color = "red")
    }


    spec_plotly <-
      est_spec %>%
      filter(f > 0) %>%
      plot_ly() %>%
      add_fun(function(p){
        p_data <- p %>% plotly_data() %>% filter(type == "fft")
        c(f_dom, fc_amp_max) %<-% (p_data %>% get_dom_freq(sample_rate))
        p %>%
          filter(type == "fft") %>%
          add_trace(
            type = "scatter", mode = "markers+text",
            x = ~f, y = ~fc_amp, color = I("blue"),
            text = ~if_else(near(fc_amp, fc_amp_max/2), str_glue("<span style='color:blue; font-weight:bold;'>{round(f_dom,1)} Hz</span>"), ""),
            textposition = "middle left",
            name = str_glue("fft; f <sub>dom</sub>: {round(f_dom, 2)} Hz"),
            hovertemplate = "Freq.: %{x:.1f} Hz"
          )
          # slice(which.max(fc_amp)) %>%
          # mutate(fc_amp = if_else(scale == "log10", log10(fc_amp), fc_amp)) %>%
          # add_annotations(
          #   x = ~f, y = ~fc_amp,
          #   axref = "x", ax = ~(f + 1), xanchor = "left",
          #   ayref = "y", ay = ~fc_amp,
          #   standoff = 5,
          #   text = ~paste(round(f,1), "Hz (fft)")
          # )
      }) %>%
      add_fun(function(p){
        p_data <- p %>% plotly_data() %>% filter(type == "spectrum")
        c(f_dom, fc_amp_max) %<-% (p_data %>% get_dom_freq(sample_rate))
        p %>%
          filter(type == "spectrum") %>%
          add_trace(
            type = "scatter", mode = "markers+text",
            x = ~f, y = ~fc_amp, color = I("black"),
            text = ~if_else(near(fc_amp, fc_amp_max/2), str_glue("<b>{round(f_dom,1)} Hz</b>"), ""),
            textposition = "middle right",
            name = str_glue("spectrum; f <sub>dom</sub>: {round(f_dom, 2)} Hz"),
            hovertemplate = "Freq.: %{x:.1f} Hz"
          )
          # slice(which.max(fc_amp)) %>%
          # mutate(fc_amp = if_else(scale == "log10", log10(fc_amp), fc_amp)) %>%
          # add_annotations(
          #   x = ~f, y = ~fc_amp, color = I("black"),
          #   axref = "x", ax = ~(f - 1), xanchor = "right",
          #   ayref = "y", ay = ~fc_amp,
          #   standoff = 5,
          #   text = ~paste(round(f,1), "Hz (spectrum)")
          # )
      }) %>%
      layout(
        legend = list(
          x = 0.8, y = 0.9,
          title = list(text = "<b>Calculation-Method</b>")
          ),
        xaxis = list(
          title = "Freq [Hz]",
          range = bp
        ),
        yaxis = list(
          title = if_else(scale == "log10", "log10(Fourier-Coef-Amp)", "Fourier-Coef-Amp"),
          type = if_else(scale == "log10", "log", "linear")
        )
      )

    if(!is.null(lfit_models)){
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          data = tibble(f = seq(bp[1], bp[2], by = 1/100)) %>% lorentz_amps(lfit_models),
          x = ~f, y = ~lf_amp, color = I("red"),
          name = str_glue("Lortentz-fit; f <sub>0</sub>: {round((lfit %>% lorentz_parameters())[['f0']], 2)} Hz"),
          hovertemplate = "Freq.: %{x:.1f} Hz"
        )
    }else{
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          x = 0, y = 1,color = I("red"),
          visible = "legendonly",
          name = "lorentz-fit did not converge"
        )
    }


    if(plot_type == "ggplot"){
      return(periodogram)
    }else{
      return(
        spec_plotly %>%
          config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "pan2d", "hoverClosestCartesian", "hoverCompareCartesian")
            )
        )
      }
  }
