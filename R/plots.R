
#' generate spectrogram-plot
#'
#' @param est_spec tibble: estimated spectrum
#' @param lfit_model return model form lorentz-fit
#'
#' @return plot-object
#'
#' @export
gen_spec_plot <-
  function(est_spec, lfit_model){
    spec_plotly <-
      est_spec %>%
      plot_ly() %>%
      add_fun(function(p){
        p_data <- p %>% plotly_data()
        c(f_dom, fc_amp_max) %<-% (p_data %>% get_dom_freq())
        p %>%
          add_trace(
            type = "scatter", mode = "markers",
            x = ~f, y = ~fc_amp, color = I("black"),
            text = ~if_else(near(fc_amp, fc_amp_max/2), str_glue("<b>{round(f_dom, 1)} Hz</b>"), ""),
            textposition = "middle right",
            name = "spectrum",
            hovertemplate = "Freq.: %{x:.1f} Hz"
          ) %>%
          slice(which.max(fc_amp)) %>%
          add_annotations(
            x = ~f, y = ~fc_amp, color = I("black"),
            axref = "x", ax = ~(f - 1), xanchor = "right",
            ayref = "y", ay = ~fc_amp,
            standoff = 10,
            text = ~ str_glue("f <sub>dom</sub>: {round(f_dom, 1)} Hz"),
            clicktoshow = "onoff",
            showarrow = FALSE
          )
      })

    if(!is.null(  lfit_model)){
      lfit <-   lfit_model
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          data = tibble(f = seq(min(est_spec$f), max(est_spec$f), by = 1/100)) %>% lorentz_amps(lfit),
          x = ~f, y = ~lf_amp, color = I("black"),
          name = "Lorentz-fit",
          hovertemplate = "Freq.: %{x:.1f} Hz"
        ) %>%
        slice(which.max(lf_amp)) %>%
        add_annotations(
          x = ~f, y = ~lf_amp, color = I("black"),
          axref = "x", ax = ~(f + 1), xanchor = "left",
          ayref = "y", ay = ~lf_amp,
          standoff = 10,
          text = ~ str_glue("f <sub>0</sub>: {round((lfit %>% lorentz_parameters())[['f0']], 1)} Hz"),
          clicktoshow = "onoff",
          showarrow = FALSE
        )
    }else{
      spec_plotly <-
        spec_plotly %>%
        add_trace(
          type = "scatter", mode = "line",
          x = 0, y = 1,color = I("blue"),
          visible = "legendonly",
          name = "lorentz-fit did not converge"
        )
    }

    spec_plotly %>%
      layout(
        legend = list(
          x = 0.8, y = 0.9,
          title = list(text = "")
        ),
        xaxis = list(
          title = "Freq [Hz]"
        )
      ) %>%
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

