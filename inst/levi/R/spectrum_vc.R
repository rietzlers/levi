# spectrum_vc.R ##

spectrumUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 1,
                 textInput(ns("spans"), label = "span", value = "c(3,3)"),
                 bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
                 numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
                 bsTooltip(ns("taper"), "apply cosine-taper to % of window", "top"),
                 actionButton(ns("add_result"), label = "add", icon = icon("plus"), width = "100%"),
                 bsTooltip(ns("add_result"), "add the current frequency-estimates to the result-data-set")
                 ),
          column(width = 5,
                 plotOutput(ns("complete_spectrum"), height = 300,
                            brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))),
          column(width = 6,
                 plotlyOutput(ns("bp_spectrum"), height = 300))
         )
    )
  )
}


spectrum_ctrl <- function(input, output, session, tapered_data, frame_rate, signal_name){

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

  bp_filtered_spectrum <-
    reactive({
      spectrum_estimate() %>% filter(f %>% between(bp()[1], bp()[2]))
    })

  lfit_model <-
    reactive({bp_filtered_spectrum() %>% fit_lorentz()})

  freq_estimates <-
    reactive({
      estimates <-
        bp_filtered_spectrum() %>%
        slice(which.max((fc_amp))) %>%
        dplyr::transmute(
          f_dom = f %>% round(2),
          f_0 = as.numeric((lfit_model() %>% lorentz_parameters())['f0']),
          d = as.numeric((lfit_model() %>% lorentz_parameters())['d']),
          spans = input$spans,
          taper = as.numeric(input$taper),
          hp_limit = bp()[2],
          lp_limit = bp()[1]
        )
    })


  # outputs  -----------
  output$complete_spectrum <- renderPlot({
    spectrum_estimate() %>%
      ggplot(aes(x = f, y = fc_amp)) +
      geom_line() +
      scale_y_continuous(
        name = "log10(spectrum-estimates)",
        trans = "log10"
      ) +
      labs(x = "Frequency [Hz]")
    })

  output$bp_spectrum <- renderPlotly({
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
              x = ~f, y = ~lf_amp, color = I("red"),
              name = "Lorentz-fit",
              hovertemplate = "Freq.: %{x:.1f} Hz"
            ) %>%
            slice(which.max(lf_amp)) %>%
            add_annotations(
              x = ~f, y = ~lf_amp, color = I("red"),
              axref = "x", ax = ~(f + 1), xanchor = "left",
              ayref = "y", ay = ~lf_amp,
              standoff = 10,
              text = ~ str_glue("<span style='color:red'> f <sub>0</sub>: {round((lfit %>% lorentz_parameters())[['f0']], 1)} Hz </span>"),
              clicktoshow = "onoff",
              showarrow = FALSE
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
    gen_spec_plot(
        bp_filtered_spectrum(),
        lfit_model = lfit_model()
      )
    })

  # return-values -----------
  list(freq_estimates =
         reactive({
           validate(need(freq_estimates, "freq_estimates"))
           freq_estimates()
         }),
       add_result = reactive(input$add_result))
}





