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
      column(width = 6, selectInput(ns("scale"), label = "", selected = "raw", choices = c("raw", "log10"))),
      column(width = 6,
             selectInput(ns("type"), label = "", selected = "spectrum", choices = c("spectrum", "fft")),
             textInput(ns("spans"), label = "span", value = "c(3,3)"),
             numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1)
             )
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
        type = input$type,
        color = "black"
        )
    })

  output$bp_spectrum <-
    renderPlotly({
      spec_plot(
        est_spec(),
        scale = input$scale,
        bp = bp(),
        sample_rate = frame_rate(),
        type = input$type,
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
spec_plot <- function(est_spec, scale = "raw", bp, type = "fft", sample_rate, color){

  bp <- round(bp, 1)
  est_spec <-
    est_spec %>%
    dplyr::filter(f %>% between(bp[1], bp[2]))

  c(f, fc_amp)  %<-% round(levi::get_dom_freq(dplyr::filter(est_spec, type == type), 1))

  c(est_params, fitted_data, lfit, sv) %<-% fit_lorentz(dplyr::filter(est_spec, type == type), sr = sample_rate)

  plot_title <- str_glue("BP: [{bp[1]}, {bp[2]}] Hz; Dom. Freq.: {f} Hz (Amp.: {fc_amp})")

  periodogram <-
    est_spec %>%
    ggplot(aes(x = f)) +
    geom_line(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), color = color) +
    geom_point(data = ~ dplyr::filter(.x, type == "spectrum"), aes(y = fc_amp), color = color, shape = "x") +
    geom_point(data = ~ dplyr::filter(.x, type == "fft", f > 0), aes(y = fc_amp), shape = "+", size = 1.5) +
    labs(
      x = "Frequency [Hz]",
      subtitle = plot_title
      )

  if(!is.null(fitted_data)){
    fitted_data <-
      tibble(f = seq(bp[1], bp[2], by = 1/100)) %>%
      mutate(lf_amp = sqrt(predict(lfit, newdata = tibble(f = f))))

    c(A, f0, d) %<-% round(abs(est_params$estimate), 2)
    print(est_params)
    periodogram <-
      periodogram +
      geom_line(data = fitted_data, aes(x = f, y = lf_amp), alpha = 0.5, color = "red") +
      labs(
        caption = str_glue("LF: A = {round(sqrt(A))}, f0 = {f} Hz, d = {d} Hz")
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




