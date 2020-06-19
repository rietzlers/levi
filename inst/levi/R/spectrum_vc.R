# spectrum_vc.R ##

spectrumUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 1,
                 actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
                 bsTooltip(ns("show_ctrls"), "Show additional controls")),
          column(width = 5,
                 plotOutput(ns("complete_spectrum"), height = 300,
                            brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))),
          column(width = 6,
                 plotlyOutput(ns("bp_spectrum"), height = 300))
         )
    ),
    bsModal(ns("additional_ctrls"), title = "Additional Controls for Spectrum-Plots", trigger = ns("show_ctrls"),
            box(
              title = tags$span("Arguments for ", tags$a(href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/spec.pgram", "spectrum ")),
              textInput(ns("spans"), label = "span", value = "c(3,3)"),
              bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
              numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
              bsTooltip(ns("taper"), "apply cosine-taper to % of window", "top")
            ),
            size = "large")
  )
}


spectrum_ctrl <- function(input, output, session, tapered_data, frame_rate, signal_name, window_range){

  # data: parameters ----
  bp <-
    reactive(({
      input$brush %>%
        levi::get_brush_range( "set band-pass by brushing spectrum-plot") %>%
        round(1)
    }))

  spans <-
    reactive({
      spans <- -1
      tryCatch(
        error = function(e) e,
        spans <- eval(rlang::parse_expr(input$spans))
      )
      validate(need(min(spans) > 1,
                    message = "No valid spans for Daniell-Smoother! It must be a numeric vector of positve integers, i.e. c(3,3), c(2)"))
      spans
    })

  taper <-
    reactive({
      validate(need(input$taper, label = "taper"))
      input$taper
    })

  # data: estimates------------
  spectrum_estimate <-
    reactive({
      levi::estimate_signal_spectrum(
        tapered_data(),
        signal_name(),
        frame_rate(),
        spans = spans(),
        taper = taper()
      )
    })

  bp_filtered_spectrum <- reactive({
    spectrum_estimate() %>% filter(f %>% between(bp()[1], bp()[2]))
  })

  lfit_models <-
    reactive({
      fitted_models =
        list(
          to_fft_data =
            levi::fit_lorentz(
              dplyr::filter(bp_filtered_spectrum(), calc_method == "fft")
            ),
          to_spectrum_data =
            levi::fit_lorentz(
              dplyr::filter(bp_filtered_spectrum(), calc_method == "spectrum")
            )
        )
    })

  parameter_estimates <-
    reactive({

      f_raw_estimates <-
        bp_filtered_spectrum() %>%
        group_by(calc_method) %>%
        get_dom_freq() %>%
        ungroup() %>%
        rowwise() %>%
        transmute(
          origin = paste0(calc_method, "_data"),
          method = "raw",
          dom_freq_estimate = f
        ) %>%
        ungroup()

      lfit_estimates <-
        lfit_models() %>%
        map_dfc(lorentz_parameters) %>%
        mutate(parameter = c("A", "f0", "d")) %>%
        pivot_longer(cols = contains("data"), names_to = "origin", values_to = "estimate", names_prefix = "to_") %>%
        mutate(method = "lorentz")

      f_lfit_estimates <-
        lfit_estimates %>%
        filter(parameter == "f0") %>%
        transmute(
          origin,
          method,
          dom_freq_estimate = estimate
        )

      damping_estimates <-
        lfit_estimates %>%
        filter(parameter == "d") %>%
        transmute(
          origin,
          method,
          damping_estimate = estimate,
          spans = input$spans,
          taper = as.numeric(input$taper)
        )



      f_raw_estimates %>%
        dplyr::union(f_lfit_estimates) %>%
        left_join(damping_estimates, by = c("origin", "method")) %>%
        mutate(
          hp_limit = bp()[1],
          lp_limit = bp()[2],
          win_start = window_range()[1],
          win_end = window_range()[2],
          t = mean(window_range(), na.rm = TRUE),
          signal = signal_name()
        ) %>%
        mutate(
          calc_method = str_replace(
            paste0(str_replace(origin, "data", ""), if_else(method == "raw", "", method)),
            "_$", ""
          )
        )

    })


  # outputs  -----------
  output$complete_spectrum <- renderPlot({
    spectrum_estimate() %>%
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
        bp_filtered_spectrum(),
        lfit_models = lfit_models(),
        scale = "raw",
        sample_rate = frame_rate()
      )
    })

  # return-values -----------
  reactive({
    validate(need(parameter_estimates, "parameter_estimates"))
    parameter_estimates()
    })
}





