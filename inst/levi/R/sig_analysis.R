# signal_plot.R #

# view --------
signalAnalysisUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    box(width = width,signalUI(ns("completeTimerange"))),
    box(width= width, plotOutput(ns("signal_in_selected_range"),  height = 150),
        title = "Signal in selected range; raw and BP-filtered(blue)", collapsible = TRUE),
    box(width = width, spectrumUI(ns("spectrum_analysis"))),
    box(width = width, resultsUI(ns("results")))
  )
}

# controller --------
signalAnalysis <- function(input, output, session, raw_tevi_data, frame_rate){
  ns <- session$ns
  # data ----
  data_selection <- reactive({
      selected_data <-brushedPoints(raw_tevi_data(), signal_brush())
      validate(need(nrow(selected_data) > 0,
                    "select data by brushing (left-click and pull) over signal-plot"))
      selected_data
    })
  observeEvent(raw_tevi_data(), {
    col_names <- raw_tevi_data() %>% names()
    signal_names <- col_names[col_names != "t"]
    updateSelectInput(session, "signal_choice", choices = signal_names)
  })

  # UIs -------------
  output$signal_in_selected_range <-
    renderPlot({
      ts_plot <- function(ds, signal_name, time_range, bp, frame_rate){
        ds %>%
          ggplot(aes(x = t)) +
          geom_line(aes(y = (.data[[signal_name]] - mean(.data[[signal_name]], na.rm = TRUE)))) +
          geom_line(data = ~ bp_filter(.x, signal_name, bp, frame_rate), aes(x = t, y = .data[[signal_name]]),
                    color = "blue", alpha = 0.5) +
          labs(
            y = signal_name
          )
      }
      ts_plot(
        ds = data_selection(),
        signal_name = signal_name(),
        time_range = get_brush_range(signal_brush()),
        bp = bp(),
        frame_rate = frame_rate()
        )
      })

  c(signal_name, signal_brush) %<-%
    callModule(signal_ctrl, "completeTimerange", raw_tevi_data, "radius_y", bp)

  c(type, bp, dom_freq, f0, d, spans, taper, add_result) %<-%
    callModule(spectrum_ctrl, "spectrum_analysis", data_selection, signal_name, frame_rate, signal_brush)

  callModule(results_ctrl, "results", signal_brush, type, bp, dom_freq, f0, d, spans, taper, add_result)
  # return-values -----------
}

