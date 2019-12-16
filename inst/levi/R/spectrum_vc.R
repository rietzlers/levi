# spectrum_vc.R ##


spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(plotlyOutput(ns("spectrum"), height = 250),
          uiOutput(ns("spectrum_info")),
          verbatimTextOutput(ns("spectrum_click_info")))
}


spectrum_ctrl <- function(input, output, session, data, signal, frame_rate){

  ns <- session$ns

  est_spec <- reactive({
    estimate_signal_spectrum(data(), signal(), frame_rate())
  })

  output$spectrum <-
    renderPlotly({
      est_spec_plot <-
        est_spec() %>%
        ggplot(aes(x = freq, y = spec)) +
        geom_line() +
        labs(x = "Frequency [Hz]",
             y = "log(periodogram)")

      est_spec_plot %>%
        ggplotly(source = ns("spectrum")) %>%
        event_register("plotly_click")


    })

  output$spectrum_info <-
    renderUI({
      max_freq <-
        est_spec() %>%
        filter(near(spec, max(spec), tol = 0.5)) %>%
        summarize(max_freq = mean(freq))
      h5(str_glue("Maximum Freq.: {round(max_freq$max_freq, 2)} Hz"))
    })

  output$spectrum_click_info <- renderPrint({
    click_info <- event_data("plotly_click", source = ns("spectrum"))
    if (is.null(click_info)) "Click events appear here (double-click to clear)" else click_info
  })

}

estimate_signal_spectrum <- function(data, signal, frame_rate) {
  est_spec <-
    spectrum(
      ts(data  %>% pull(!!signal), frequency = frame_rate))

  tibble(freq = est_spec$freq, spec = log(est_spec$spec))
}
