# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 6,
            plotOutput(ns("complete_spectrum"), height = 250,
               brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))
            ),
        column(width = 6,
            plotlyOutput(ns("bp_spectrum"), height = 250)
        )
    ),
    fluidRow(
      column(3, textOutput(ns("dom_freq"))),
      column(3, textOutput(ns("f0"))),
      column(3, textOutput(ns("d")))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_model, data_selection, signal_name,
                          selected_tab, spectrum_view_UI, tasks, notifications){

  # data ----
  est_spec <- reactive({
    levi::estimate_signal_spectrum(
      data_selection(),
      signal_name(),
      tevi_model()$frame_rate,
      spans = spans(),
      taper = taper()
    )
  })
  lfit <- reactive({
    lfit <-
      levi::fit_lorentz(
      dplyr::filter(est_spec(), type == input$type),
      bp = bp(),
      sr = tevi_model()$frame_rate)
    notifications_list <- notifications()
    if(is.null(lfit)){
      notifications_list[["lfit"]] <- notificationItem("lorentz-fit did not succed", icon = icon("frown"), status = "warning")
    }else{
      notifications_list[["lfit"]] <- NULL
    }
    notifications(notifications_list)
    lfit
  })
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

  dom_freq <- reactive({
    f_dom <- NULL
    c(f_dom, ...)  %<-%
      (est_spec() %>%
         dplyr::filter(type == input$type, f %>% between(bp()[1], bp()[2])) %>%
         levi::get_dom_freq(sample_rate = tevi_model()$frame_rate) %>%
        round(2))
    validate(need(f_dom, "f_dom"))
    f_dom
  })
  f0 <- reactive({
    validate(need(lfit(), message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit())[2]
  })
  d <- reactive({
    validate(need(lfit(), message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit())[3]
  })

  # UI-ctrls -----------
  observeEvent(selected_tab(),{
    if (selected_tab() == "signalAnalysis"){
      spectrum_view_UI(
        box(width = 12, title = "Spec/FFT-Controls", collapsible = TRUE, collapsed = TRUE,
            selectInput(session$ns("scale"), label = "", selected = "log10", choices = c("raw", "log10")),
            bsTooltip(session$ns("scale"), "scaling of spectrogram-ordinate", "top"),
            selectInput(session$ns("type"), label = "", selected = "spectrum", choices = c("spectrum", "fft")),
            bsTooltip(session$ns("type"), "use FFT/spectrogram to calculate periodogram", "top"),
            textInput(session$ns("spans"), label = "span", value = "c(3,3)"),
            bsTooltip(session$ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
            numericInput(session$ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
            bsTooltip(session$ns("taper"), "apply cosine-taper to % of window", "top")
        ))
    }else{
      spectrum_view_UI(NULL)
    }
  }) # dynamic-sidebar UI
  output$dom_freq <- renderText({str_glue("Dom.Freq: {dom_freq()}")})
  output$f0 <- renderText({str_glue("f0: {f0()}")})
  output$d <- renderText({str_glue("D: {d()}")})

  output$complete_spectrum <- renderPlot({
      spec_plot(
        est_spec(),
        lfit = NULL,
          # levi::fit_lorentz(
          # dplyr::filter(est_spec(), type == input$type),
          # bp = c(0, tevi_model()$frame_rate/2),
          # sr = tevi_model()$frame_rate),
        scale = input$scale,
        bp = c(0, tevi_model()$frame_rate/2),
        sample_rate = tevi_model()$frame_rate,
        type_choosen= input$type
        )
    })
  output$bp_spectrum <- renderPlotly({
      spec_plot(
        est_spec(),
        lfit = lfit(),
        scale = input$scale,
        bp = bp(),
        sample_rate = tevi_model()$frame_rate,
        type_choosen =  input$type
      ) %>%
        ggplotly()
    })
  # return-values -----------
  list(
    type = reactive(input$type),
    bp = bp,
    dom_freq = dom_freq,
    f0 = f0,
    d = d,
    spans = reactive(input$spans),
    taper = reactive(input$taper)
  )

}

# helper-functions ----------
spec_plot <-
  function(est_spec, lfit, scale = "log10", bp, type_choosen = "fft", sample_rate){
  periodogram <-
    est_spec %>%
    filter(f %>% between(bp[1], bp[2])) %>%
    ggplot(aes(x = f)) +
    geom_line(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp)) +
    geom_point(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), shape = "x", size = 0.8) +
    geom_point(data = ~ dplyr::filter(.x, type == "fft", f > 0), aes(y = fc_amp), color = "blue", shape = "+", size = 1.5) +
    labs(x = "Frequency [Hz]")

  if(!is.null(lfit)){
    fitted_data <-
      tibble(f = seq(bp[1], bp[2], by = 1/100)) %>%
      lorentz_amps(lfit)

    periodogram <-
      periodogram +
      geom_line(data = fitted_data, aes(x = f, y = lf_amp), alpha = 0.5, color = "red")
  } # add lorentz-curve
  if(scale == "raw"){
    return(periodogram + labs(y = scale))
    }
  if(scale == "log10"){
    return(
      periodogram +
        labs(y = scale) +
        scale_y_continuous(trans = scale)
    )
  }
}


