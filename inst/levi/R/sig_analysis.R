# signal_plot.R #

# view --------
signalAnalysisUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    box(width = width, signalUI(ns("completeTimerange"))),
    box(width = width / 2,
        plotOutput(ns("signal_in_selected_range"),  height = 250),
        uiOutput(ns("selected_signal_info"))),
    box(width = width / 2, spectrumUI(ns("spectrum")))
  )
}

# controller --------
signalAnalysis <- function(input, output, session, raw_tevi_data, frame_rate){

  ns <- session$ns

  data_selection <-
    reactive({
      selected_data <-brushedPoints(raw_tevi_data(), signal_brush())
      validate(need(nrow(selected_data) > 0,
                    "select data by brushing (left-click and pull) over signal-plot"))
      selected_data
    })

  # update UI -
  observeEvent(raw_tevi_data(), {
    col_names <- raw_tevi_data() %>% names()
    signal_names <- col_names[col_names != "time"]
    updateSelectInput(session, "signal_choice", choices = signal_names)
  })


  output$signal_in_selected_range <- renderPlot({ts_plot(data_selection(), signal_sym())})

  output$selected_signal_info <-
    renderUI({
      br <- get_brush_range(input$brush)
      h5(str_glue("Window center-point: {round(mean(br), 2)} s and window-width: {round(diff(br), 2)} s"))
    })

  # submodules ----------
  c(signal_sym, signal_brush) %<-% callModule(signal_ctrl, "completeTimerange", raw_tevi_data, "radius_y")

  callModule(spectrum_ctrl, "spectrum", data_selection, signal_sym, frame_rate)

 # return-values -----------
}

# helpers -----------------
ts_plot <- function(ds, var){
  ds %>%
    ggplot(aes(x = time)) +
    geom_line(aes(y = !!var))
}
