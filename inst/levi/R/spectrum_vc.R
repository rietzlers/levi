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
            plotOutput(ns("bp_spectrum"), height = 250)
        )
    ),
    fluidRow(
      column(width = 6, selectInput(ns("scale"), label = "", selected = "raw", choices = c("raw", "log10"))),
      column(width = 6, selectInput(ns("type"), label = "", selected = "spectrum", choices = c("spectrum", "fft")))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, data_selection, signal_name, frame_rate){

  ns <- session$ns

  # data ----
  est_spec <- reactive({
    estimate_signal_spectrum(data_selection(), signal_name(), frame_rate(), input$type)
  })

  bp <- reactive(({
    get_brush_range(input$brush, "set band-pass by brushing spectrum-plot")
  }))

  # output-ctrls -----------
  output$complete_spectrum <-
    renderPlot({
      spec_plot(
        est_spec(),
        scale = input$scale,
        bp = c(0, frame_rate()/2),
        sample_rate = frame_rate(),
        color = "black"
        )
    })

  output$bp_spectrum <-
    renderPlot({
      spec_plot(
        est_spec(),
        scale = input$scale,
        bp = bp(),
        sample_rate = frame_rate(),
        color = "blue"
      )
    })

  # return-values -----------
  list(
    bp
  )

}

# helper-functions ----------
spec_plot <- function(est_spec, scale = "raw", bp, sample_rate, color){

  bp <- round(bp, 1)
  est_spec <-
    est_spec %>%
    filter(f %>% between(bp[1], bp[2]))

  c(f, fc_amp)  %<-% round(levi::get_dom_freq(est_spec), 1)

  c(est_params, fitted_data, lfit, sv) %<-%
    fit_lorentz(est_spec, sr = sample_rate)

  plot_title <- str_glue("BP: [{bp[1]}, {bp[2]}] Hz; Dom. Freq.: {f} Hz (Amp.: {fc_amp})")
  periodogram <-
    est_spec %>%
    ggplot(aes(x = f, y = fc_amp)) +
    geom_line(color = color) +
    labs(
      x = "Frequency [Hz]",
      subtitle = plot_title
      )

  if(!is.null(fitted_data)){
    # fitted_data <-
    #   tibble(f = seq(bp[1], bp[2], by = 1000)) %>%
    #   mutate(lf_amp = sqrt(predict(lfit, newdata = list(f = f))))
    c(A, f0, d) %<-% round(abs(est_params$estimate), 2)
    print(est_params)
    periodogram <-
      periodogram +
      geom_line(data = fitted_data, aes(x = f, y = lf_amp), alpha = 0.5, color = "red") +
      labs(
        caption = str_glue("LF: A = {A}, f0 = {f} Hz, d = {d} Hz")
      )
  }else{
    periodogram <-
      periodogram +
      labs(
        caption = str_glue("LF: did not converge")
      )
  }
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


