
#' generate spectrogram-plot
#'
#' @param est_spec tibble: estimated spectrum
#' @param lfit_models lfit-models
#' @param scale string: raw/log10
#' @param sample_rate sample-rate
#'
#' @return plot-object
#'
#' @export
gen_spec_plot <-
  function(est_spec, lfit_models, scale = "log10",  sample_rate){
    spec_plotly <-
      est_spec %>%
      plot_ly() %>%
      add_fun(function(p){
        p_data <- p %>% plotly_data() %>% filter(calc_method == "fft")
        c(f_dom, fc_amp_max) %<-% (p_data %>% get_dom_freq(sample_rate))
        p %>%
          filter(calc_method == "fft") %>%
          add_trace(
            type = "scatter", mode = "markers",
            x = ~f, y = ~fc_amp, color = I("blue"),
            text = ~if_else(near(fc_amp, fc_amp_max/2), str_glue("<span style='color:blue; font-weight:bold;'>{round(f_dom, 1)} Hz</span>"), ""),
            textposition = "middle left",
            name = str_glue("fft; f <sub>dom</sub>: {round(f_dom, 1)} Hz"),
            hovertemplate = "Freq.: %{x:.1f} Hz"
          ) %>%
          slice(which.max(fc_amp)) %>%
          mutate(fc_amp = if_else(scale == "log10", log10(fc_amp), fc_amp)) %>%
          add_annotations(
            x = ~f, y = ~fc_amp,
            axref = "x", ax = ~(f + 1), xanchor = "left",
            ayref = "y", ay = ~fc_amp,
            standoff = 5,
            text = ~paste(round(f,1), "Hz (fft)"),
            clicktoshow = "onoff"
          )
      }) %>%
      add_fun(function(p){
        p_data <- p %>% plotly_data() %>% filter(calc_method == "spectrum")
        c(f_dom, fc_amp_max) %<-% (p_data %>% get_dom_freq(sample_rate))
        p %>%
          filter(calc_method == "spectrum") %>%
          add_trace(
            type = "scatter", mode = "markers",
            x = ~f, y = ~fc_amp, color = I("black"),
            text = ~if_else(near(fc_amp, fc_amp_max/2), str_glue("<b>{round(f_dom, 1)} Hz</b>"), ""),
            textposition = "middle right",
            name = str_glue("spectrum; f <sub>dom</sub>: {round(f_dom, 1)} Hz"),
            hovertemplate = "Freq.: %{x:.1f} Hz"
          ) %>%
          slice(which.max(fc_amp)) %>%
          mutate(fc_amp = if_else(scale == "log10", log10(fc_amp), fc_amp)) %>%
          add_annotations(
            x = ~f, y = ~fc_amp, color = I("black"),
            axref = "x", ax = ~(f - 1), xanchor = "right",
            ayref = "y", ay = ~fc_amp,
            standoff = 5,
            text = ~paste(round(f,1), "Hz (spectrum)"),
            clicktoshow = "onoff"
          )
      }) %>%
      layout(
        legend = list(
          x = 0.8, y = 0.9,
          title = list(text = "<b>Calculation-Method</b>")
        ),
        xaxis = list(
          title = "Freq [Hz]"
        ),
        yaxis = list(
          title = if_else(scale == "log10", "log10(Fourier-Coef-Amp)", "Fourier-Coef-Amp"),
          type = if_else(scale == "log10", "log", "linear")
        )
      )

    if(!is.null(  lfit_models$to_fft_data)){
      lfit <-   lfit_models$to_fft_data
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          data = tibble(f = seq(min(est_spec$f), max(est_spec$f), by = 1/100)) %>% lorentz_amps(lfit),
          x = ~f, y = ~lf_amp, color = I("blue"),
          name = str_glue("Lortentz-fit (fft); f <sub>0</sub>: {round((lfit %>% lorentz_parameters())[['f0']], 1)} Hz"),
          hovertemplate = "Freq.: %{x:.1f} Hz"
        )
    }else{
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          x = 0, y = 1,color = I("blue"),
          visible = "legendonly",
          name = "lorentz-fit (fft) did not converge"
        )
    }

    if(!is.null(  lfit_models$to_spectrum_data)){
      lfit <-   lfit_models$to_spectrum_data
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          data = tibble(f = seq(min(est_spec$f), max(est_spec$f), by = 1/100)) %>% lorentz_amps(lfit),
          x = ~f, y = ~lf_amp, color = I("black"),
          name = str_glue("Lortentz-fit (spectrum); f <sub>0</sub>: {round((lfit %>% lorentz_parameters())[['f0']], 1)} Hz"),
          hovertemplate = "Freq.: %{x:.1f} Hz"
        )
    }else{
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          x = 0, y = 1,color = I("blue"),
          visible = "legendonly",
          name = "lorentz-fit (spectrum) did not converge"
        )
    }

    spec_plotly %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoomIn2d",
          "zoomOut2d",
          "lasso2d",
          "pan2d",
          "hoverClosestCartesian"
        )
      )

  }


#' adds a trace to st-result-plot for a calc_method
#'
#' @param p plot_ly object
#' @param cm string specifying the calc_method
#' @param trace name/legend for the trace
#' @param estimates data-frame
#' @param p_size point-size
#' @param sl logical: showlegend
#' @param fit_lm logical: add lm-model-fit to trace
#' @param farbe color of the trace
#'
#' @return plot_ly-object
#' @export
calc_method_trace <- function(p, cm, trace,  estimates = NULL, p_size = 6,  sl = TRUE, fit_lm = FALSE, farbe = "black"){
  if(missing(trace)){trace = cm}
  p <-
    p %>%
    filter(calc_method == cm) %>%
    add_trace(
      data = estimates,
      name = trace,
      type = "scatter",
      mode = "markers",
      x = ~ get(x_var),
      y = ~ get(y_var),
      hovertemplate = paste("%{y:.2f}", y_unit),
      size = I(p_size),
      showlegend = sl,
      color = I(farbe)
    )
  if(fit_lm == TRUE){
    complete_obs <-
      plotly_data(p) %>%
      filter(!is.na(dom_freq_estimate)) %>%
      filter(calc_method == cm)
    lm <- lm(get(y_var) ~ get(x_var), data = complete_obs)
    c(y0, m) %<-% coef(lm)

    lm_data <-
      tibble(
        x = complete_obs[[x_var]],
        fitted_values = predict(lm)
      )
    p <-
      p %>%
      add_lines(
        data = lm_data,
        name = str_glue("{round(y0, 2)} + {round(m, 4)} x"),
        x = ~x, y = ~fitted_values,
        hoverinfo = "none",
        color = I(farbe)
      )
  }
  p
}
