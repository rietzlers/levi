# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(
    plotOutput(ns("spectrum_plot"), height = 250,
               brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
    uiOutput(ns("spectrum_info")),
    fluidRow(
      column(width = 6, selectInput(ns("scale"), label = "", selected = "raw", choices = c("raw", "log10"))),
      column(width = 6, selectInput(ns("type"), label = "", selected = "spectrum", choices = c("spectrum", "fft")))
    ),
    # fluidRow(
    #   column(width = 4, numericInput(ns("bp_low"),  label = "", value = 0, min = 0, step = 1)),
    #   column(width = 4, numericInput(ns("bp_high"), label = "", value = 200))
    # ),
    verbatimTextOutput(ns("select_info")),
    verbatimTextOutput(ns("click_info"))
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_data, signal_name, frame_rate){

  ns <- session$ns

  # data ----
  est_spec <- reactive({
    estimate_signal_spectrum(tevi_data(), signal_name(), frame_rate(), input$type)
  })

  bp <- reactive(({
    get_brush_range(input$brush, "set band-pass by brushing spectrum-plot")
  }))

  # output-ctrls -----------
  output$spectrum_plot <-
    renderPlot({
      spec_plot(est_spec(), scale = input$scale)
    })

  output$spectrum_info <- renderUI({
    c(f, fc_amp)  %<-% levi::get_dom_freq(est_spec())
    h5(str_glue("Dominant Freq.: {round(f, 2)} Hz/ Amp.: {round(fc_amp, 2)}"))
  })

  # return-values -----------
  list(
    bp
  )

}

# helper-functions ----------
spec_plot <- function(est_spec, scale = "raw"){

  periodogram <-
    est_spec %>%
    ggplot(aes(x = f, y = spec)) +
    geom_line() +
    labs(x = "Frequency [Hz]")

  if(scale == "raw"){
    return(
      periodogram +
        labs(y = scale)
    )
  }

  if(scale == "log10"){
    return(
      periodogram +
        labs(y = scale) +
        scale_y_continuous(trans = scale)
    )
  }

}

estimate_signal_spectrum <- function(signal_data, signal_name, frame_rate, type = "spectrum") {
  if(type == "spectrum"){
    est_spec <-
      spectrum(
        ts(signal_data[[signal_name]], frequency = frame_rate),
        plot = FALSE)
    return(
      tibble(f = est_spec$freq,  spec = est_spec$spec, fc_amp = sqrt(spec / length(f))))
  }

  if(type == "fft"){
    return(
      levi::fftc(signal_data, signal_name, sr = frame_rate) %>%
        filter(f < frame_rate/2)
    )
  }


}


