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
      column(width = 3,
             numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, data_selection, signal_name, frame_rate){

  ns <- session$ns

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
      get_brush_range(input$brush, "set band-pass by brushing spectrum-plot") %>%
        round(1)
  }))

  # output-ctrls -----------
  output$complete_spectrum <-
    renderPlot({
      spec_plot(
        est_spec(),
        lfit =
          levi::fit_lorentz(dplyr::filter(est_spec(), type == input$type),
                            bp = c(0, frame_rate()/2),
                            sr = frame_rate()),
        scale = input$scale,
        bp = c(0, frame_rate()/2),
        sample_rate = frame_rate(),
        type_choosen= input$type,
        color = "black"
        )
    })

  output$bp_spectrum <-
    renderPlotly({
      spec_plot(
        est_spec(),
        lfit = lfit(),
        scale = input$scale,
        bp = bp(),
        sample_rate = frame_rate(),
        type_choosen =  input$type,
        color = "blue"
      ) %>%
        ggplotly()
    })

  # return-values -----------
  list(
    bp
  )

}

# helper-functions ----------
spec_plot <- function(est_spec, lfit, scale = "log10", bp, type_choosen = "fft", sample_rate, color){

  numerical_summary(est_spec, lfit, sample_rate)

  periodogram <-
    est_spec %>%
    ggplot(aes(x = f)) +
    geom_line(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), color = color) +
    geom_point(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), color = color, shape = "x", size = 0.8) +
    geom_point(data = ~ dplyr::filter(.x, type == "fft", f > 0), aes(y = fc_amp), shape = "+", size = 1.5) +
    labs(x = "Frequency [Hz]") +
    xlim(bp)


  if(!is.null(lfit)){
    fitted_data <-
      tibble(f = seq(bp[1], bp[2], by = 1/100)) %>%
      lorentz_amps(lfit)

    periodogram <-
      periodogram +
      geom_line(data = fitted_data, aes(x = f, y = lf_amp), alpha = 0.5, color = "red")
  }

  if(scale == "raw"){return(periodogram + labs(y = scale))}
  if(scale == "log10"){
    return(
      periodogram +
        labs(y = scale) +
        scale_y_continuous(trans = scale)
    )
  }
}

numerical_summary <- function(est_spec, lfit, sample_rate){
  c(f_dom, fc_amp_dom)  %<-%
    round(
      levi::get_dom_freq(
        est_spec %>% dplyr::filter(type == "fft"),
        sample_rate = sample_rate)
      , 1)

  if(!is.null(lfit)){
    print(lorentz_parameters(lfit))
    #print(summary(lfit))
  }
}


