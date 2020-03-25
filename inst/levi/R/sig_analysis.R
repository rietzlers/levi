
signalAnalysisUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("complete_signal"), height = 200,
                   brush = brushOpts(id = ns("signal_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
    box(width = width, spectrumUI(ns("spectrum_analysis"))),
    box(width = width, resultsUI(ns("results"))),
    box(width= width, title = "Signal in selected range; raw and BP-filtered(blue)", collapsible = TRUE, {
      plotOutput(ns("signal_in_selected_range"),  height = 150)}),
  )
}

signalAnalysis <- function(input, output, session,
                           tevi_model, sample_specs, selected_tab,
                           signal_selection_UI, signal_view_UI, spectrum_view_UI, spectrum_results_UI,
                           tasks, notifications){
  # data ----
  data_selection <- reactive({
      selected_data <-brushedPoints(tevi_model()$tevi_data, input$signal_brush)
      validate(need(nrow(selected_data) > 0,
                    "choose time-window by brushing (left-click and pull) over signal-plot"))
      selected_data
    })
  observeEvent({selected_tab()},{
                 if (!(selected_tab() %in% c("data_setup", "importTeviData", "setup_sample_specs"))) {
                   signal_selection_UI(div(
                       selectInput(session$ns("selected_signal"),
                                   label = "Select Signal to analyse",
                                   choices = names(tevi_model()$tevi_data),
                                   selected = "radius_y")))
                 } else{
                   signal_selection_UI(NULL)
                 }
               }) #update signal-selection

  time_range <- reactive({get_brush_range(input$signal_brush, "set time range in Signal Analysis")})
  signal_name  = reactive({input$selected_signal})

  # UIs -------------
  output$complete_signal <- renderPlot({
    validate(need(input$selected_signal, label = "signal"))
    sig_plot <-
      tevi_model()$tevi_data %>%
      ggplot(aes(x = t)) +
      geom_line(aes_string(y = input$selected_signal))
    if(is.null(need(tevi_model()$HPs, "heat-pulses"))){
      sig_plot <- sig_plot + geom_vline(xintercept = tevi_model()$HPs %>% flatten_dbl(), linetype = "dashed", color = "red")
    }
    if(is.null(need(tevi_model()$exp_time_range, "exp_time_range"))){
      sig_plot <- sig_plot + geom_vline(xintercept = tevi_model()$exp_time_range, linetype = "dashed", color = "blue")
    }
    sig_plot
  })
  output$signal_in_selected_range <- renderPlot({
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
      {ts_plot(
        ds = data_selection(),
        signal_name = signal_name(),
        time_range = get_brush_range(signal_brush()),
        bp = bp(),
        frame_rate = tevi_model()$frame_rate
        )} # call ts_plot with reactives
      })

  c(type, bp, dom_freq, f0, d, spans, taper) %<-%
    callModule(spectrum_ctrl, "spectrum_analysis", tevi_model, data_selection, signal_name,
               selected_tab, spectrum_view_UI, tasks, notifications)

  callModule(results_ctrl, "results",
             tevi_model, sample_specs, data_selection, time_range, signal_name,
             type, bp, dom_freq, f0, d, spans, taper,
             selected_tab, spectrum_results_UI)

  # return-values ----------
  reactive({
    list(
      selected_signal = signal_name(),
      time_range = time_range(),
      bp = bp()
    )
  })
}

