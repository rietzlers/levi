# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Spectrogram",
        fluidRow(
          column(width = 6,
                 plotOutput(ns("complete_spectrum"), height = 250,
                            brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))),
          column(width = 6, plotlyOutput(ns("bp_spectrum"), height = 250))
        )
      ),
      tabPanel("Controls",
               selectInput(ns("scale"), label = "Set scaling of spectrogram ordinate", selected = "log10", choices = c("raw", "log10")),
               bsTooltip(ns("scale"), "scaling of spectrogram-ordinate", "top"),
               selectInput(ns("calc_method"), label = "Choose method to calculate Foruier-Coefficients", selected = "spectrum", choices = c("spectrum", "fft")),
               bsTooltip(ns("calc_method"), "use FFT/spectrogram to calculate periodogram", "top"),
               textInput(ns("spans"), label = "span", value = "c(3,3)"),
               bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
               numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
               bsTooltip(ns("taper"), "apply cosine-taper to % of window", "top"))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_model, data_selection, signal_name,
                          selected_tab, spectrum_view_UI, tasks, notifications){

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
      dplyr::filter(spectrum_estimate(), type == "fft"),
      bp = bp(), # lorentz-kurve wird IMMER an die BP-gefilterte Kurve angepasst!
      sr = tevi_model()$frame_rate)
    lorentz_fit_to_spectrum <-
      levi::fit_lorentz(
        dplyr::filter(spectrum_estimate(), type == "spectrum"),
        bp = bp(), # lorentz-kurve wird IMMER an die BP-gefilterte Kurve angepasst!
        sr = tevi_model()$frame_rate)
    fitted_models = list(
      to_fft_data = lorentz_fit_to_fft,
      to_spectrum_data = lorentz_fit_to_spectrum
    )
  })
  dom_freq_estimates <- reactive({

  })

  damping_estimate <- reactive({

  })
  dom_freq <- reactive({
    f_dom <- NULL
    c(f_dom, ...)  %<-%
      (spectrum_estimate() %>%
         dplyr::filter(type == input$calc_method, f %>% between(bp()[1], bp()[2])) %>%
         levi::get_dom_freq(sample_rate = tevi_model()$frame_rate) %>%
        round(2))
    validate(need(f_dom, "f_dom"))
    f_dom
  })

  f0 <- reactive({
    validate(need(lfit_models()$to_fft_data, message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit_models()$to_fft_data)[2]
  })
  d <- reactive({
    validate(need(lfit_models()$to_fft_data, message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit_models()$to_fft_data)[3]
  })

  # outputs  -----------
  output$complete_spectrum <- renderPlot({
    spectrum_estimate() %>%
      filter(f %>% between(0, tevi_model()$frame_rate/2)) %>%
      ggplot(aes(x = f)) +
      geom_line(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp)) +
      geom_point(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), shape = "x", size = 0.8) +
      geom_point(data = ~ dplyr::filter(.x, type == "fft", f > 0), aes(y = fc_amp), color = "blue", shape = "+", size = 1.5) +
      scale_y_continuous(
        name = if_else(input$scale == "log10", "log10(Fourier-Coef-Amp)", "Fourier-Coef-Amp"),
        trans = if_else(input$scale == "log10", "log10", "identity")
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
  list(
    type = reactive(input$calc_method),
    bp = bp,
    dom_freq = dom_freq,
    f0 = f0,
    d = d,
    spans = reactive(input$spans),
    taper = reactive(input$taper)
  )

}





