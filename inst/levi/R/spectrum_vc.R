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
      column(width = 3, selectInput(ns("scale"), label = "", selected = "log10", choices = c("raw", "log10"))),
      column(width = 3, selectInput(ns("type"), label = "", selected = "spectrum", choices = c("spectrum", "fft"))),
      column(width = 3,
             textInput(ns("spans"), label = "span", value = "c(3,3)"),
             bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body"))),
      column(width = 3, numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1))
    ),
    fluidRow(
      column(3, textOutput(ns("dom_freq"))),
      column(3, textOutput(ns("f0"))),
      column(3, textOutput(ns("d"))),
      column(width = 3, actionButton(ns("save_result"), label = "save result"))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, data_selection, signal_name, frame_rate, time_range){
  # data ----
  est_spec <- reactive({
    levi::estimate_signal_spectrum(
      data_selection(),
      signal_name(),
      frame_rate(),
      spans = eval(rlang::parse_expr(input$spans)),
      taper = input$taper
    )
  })
  lfit <- reactive({
    levi::fit_lorentz(
      dplyr::filter(est_spec(), type == input$type),
      bp = bp(),
      sr = frame_rate())
  })
  bp <- reactive(({
    input$brush %>%
      levi::get_brush_range( "set band-pass by brushing spectrum-plot") %>%
      round(1)
  }))
  dom_freq <- reactive({
    c(f_dom, ...)  %<-%
      (est_spec() %>%
         dplyr::filter(type == input$type, f %>% between(bp()[1], bp()[2])) %>%
         levi::get_dom_freq(sample_rate = frame_rate()) %>%
        round(2))
    f_dom
  })
  f0 <- reactive({
    if(!is.null(lfit())){
      params <- lorentz_parameters(lfit())
    }else{
      params <- c(A = NA_real_, f0 = NA_real_, d = NA_real_)
    }
    params[2]
  })
  d <- reactive({
    if(!is.null(lfit())){
      params <- lorentz_parameters(lfit())
    }else{
      params <- c(A = NA_real_, f0 = NA_real_, d = NA_real_)
    }
    params[3]
  })

  # output-ctrls -----------
  output$dom_freq <- renderText({str_glue("Dom.Freq: {dom_freq()}")})
  output$f0 <- renderText({str_glue("f0: {f0()}")})
  output$d <- renderText({str_glue("D: {d()}")})

  output$complete_spectrum <- renderPlot({
      spec_plot(
        est_spec(),
        lfit = NULL,
          # levi::fit_lorentz(
          # dplyr::filter(est_spec(), type == input$type),
          # bp = c(0, frame_rate()/2),
          # sr = frame_rate()),
        scale = input$scale,
        bp = c(0, frame_rate()/2),
        sample_rate = frame_rate(),
        type_choosen= input$type
        )
    })
  output$bp_spectrum <- renderPlotly({
      spec_plot(
        est_spec(),
        lfit = lfit(),
        scale = input$scale,
        bp = bp(),
        sample_rate = frame_rate(),
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
    taper = reactive(input$taper),
    add_result = reactive(input$save_result)
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


