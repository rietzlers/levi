
signalAnalysisUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    box(width = width,  title = "Spectrogram", collapsible = TRUE, collapsed = FALSE, spectrumUI(ns("spectrum_analysis"))),
    box(width = width, resultsUI(ns("results")))
  )
}

signalAnalysis <- function(input, output, session, tevi_model, sample_specs, tasks, notifications){


  # module-calls --------
  c(data_selection, signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  parameter_estimates <- callModule(spectrum_ctrl, "spectrum_analysis", tevi_model, data_selection, signal_name, window_range)

  # callModule(results_ctrl, "results",
  #            tevi_model, sample_specs, data_selection, window_range, signal_name,
  #            parameter_estimates)

  # return-values ----------
  reactive({
    list(
      selected_signal = signal_name(),
      window_range = window_range(),
      bp = bp()
    )
  })
}

