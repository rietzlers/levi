# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
        fluidRow(
          column(width = 1,
                 selectInput(ns("scale"), label = "Set scaling of spectrogram ordinate", selected = "log10", choices = c("raw", "log10")),
                 bsTooltip(ns("scale"), "scaling of spectrogram-ordinate", "top"),
                 textInput(ns("spans"), label = "span", value = "c(3,3)"),
                 bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
                 numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
                 bsTooltip(ns("taper"), "apply cosine-taper to % of window", "top")),
          column(width = 5,
                 plotOutput(ns("complete_spectrum"), height = 250,
                            brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))),
          column(width = 6,
                 plotlyOutput(ns("bp_spectrum"), height = 250))
        ),
    style = "border-style = groove; border: 1px solid black; padding: 25px"
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_model, data_selection, signal_name, window_range){

  # data: parameters ----
  bp <- reactive(({
    input$brush %>%
      levi::get_brush_range( "set band-pass by brushing spectrum-plot") %>%
      round(1)
  }))
  spans <- reactive({
    spans <- -1
    tryCatch(
      error = function(e) e,
      spans <- eval(rlang::parse_expr(input$spans))
    )
    validate(need(min(spans) > 1,
                  message = "No valid spans for Daniell-Smoother! It must be a numeric vector of positve integers, i.e. c(3,3), c(2)"))
    spans
  })
  taper <- reactive({
    validate(need(input$taper, label = "taper"))
    input$taper
  })
  # data: estimates------------
  spectrum_estimate <- reactive({
    levi::estimate_signal_spectrum(
      data_selection(),
      signal_name(),
      tevi_model()$frame_rate,
      spans = spans(),
      taper = taper()
    )
  })
  lfit_models <- reactive({
    lorentz_fit_to_fft <-
      levi::fit_lorentz(
      dplyr::filter(spectrum_estimate(), calc_method == "fft"),
      bp = bp(), # lorentz-kurve wird IMMER an die BP-gefilterte Kurve angepasst!
      sr = tevi_model()$frame_rate)
    lorentz_fit_to_spectrum <-
      levi::fit_lorentz(
        dplyr::filter(spectrum_estimate(), calc_method == "spectrum"),
        bp = bp(), # lorentz-kurve wird IMMER an die BP-gefilterte Kurve angepasst!
        sr = tevi_model()$frame_rate)
    fitted_models = list(
      to_fft_data = lorentz_fit_to_fft,
      to_spectrum_data = lorentz_fit_to_spectrum
    )
  })
  parameter_estimates <- reactive({
    sr <- tevi_model()$frame_rate

    f_raw_estimates <-
      spectrum_estimate() %>%
      group_by(calc_method) %>%
      get_dom_freq(sr) %>%
      ungroup() %>%
      transmute(
        calc_method,
        dom_freq_estimate = f
      )

    lfit_estimates <-
     lfit_models() %>%
      map_dfr(lorentz_parameters) %>%
      set_names(c("fft_lorentz", "spectrum_lorentz")) %>%
      mutate(parameter = c("A", "f0", "d")) %>%
      pivot_longer(cols = contains("lorentz"), names_to = "calc_method")

    f_lfit_estimates <-
      lfit_estimates %>%
      filter(parameter == "f0") %>%
      transmute(
        calc_method,
        dom_freq_estimate = value
      )

    damping_estimates <-
      lfit_estimates %>%
      filter(parameter == "d") %>%
      transmute(
        calc_method,
        damping_estimate = value,
        spans = input$spans,
        taper = as.numeric(input$taper)
      )

    f_raw_estimates %>%
      dplyr::union(f_lfit_estimates) %>%
      left_join(damping_estimates, by = c("calc_method")) %>%
      mutate(
        hp_limit = bp()[1],
        lp_limit = bp()[2],
        win_start = window_range()[1],
        win_end = window_range()[2],
        t = mean(window_range(), na.rm = TRUE),
        signal = signal_name()
      )

  })


  # outputs  -----------
  output$complete_spectrum <- renderPlot({
    spectrum_estimate() %>%
      filter(f %>% between(0, tevi_model()$frame_rate/2)) %>%
      ggplot(aes(x = f)) +
      geom_line(data = ~ dplyr::filter(.x, calc_method == "spectrum"), aes(y = fc_amp)) +
      geom_point(data = ~ dplyr::filter(.x, calc_method == "spectrum"), aes(y = fc_amp), shape = "x", size = 0.8) +
      geom_point(data = ~ dplyr::filter(.x, calc_method == "fft", f > 0), aes(y = fc_amp), color = "blue", shape = "+", size = 1.5) +
      scale_y_continuous(
        name = "log10(Fourier-Coef-Amp)", # if_else(input$scale == "log10", "log10(Fourier-Coef-Amp)", "Fourier-Coef-Amp"),
        trans = "log10"#if_else(input$scale == "log10", "log10", "identity")
      ) +
      labs(x = "Frequency [Hz]")
    })

  output$bp_spectrum <- renderPlotly({
      gen_spec_plot(
        spectrum_estimate(),
        lfit_models = lfit_models(),
        scale = input$scale,
        bp = bp(),
        sample_rate = tevi_model()$frame_rate
      )
    })

  # return-values -----------
  parameter_estimates

}





