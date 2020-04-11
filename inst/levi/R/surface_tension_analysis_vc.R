
surface_tension_analysis_UI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    box(width = width,  title = "Spectrogram", collapsible = TRUE, collapsed = FALSE, spectrumUI(ns("spectrum_analysis"))),
    box(width = width, surface_tension_results_UI(ns("results")))
  )
}

surface_tension_analysis_ctrl <- function(input, output, session, tevi_model, sample_specs, tasks, notifications){


  # module-calls --------
  c(data_selection, signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  parameter_estimates <- callModule(spectrum_ctrl, "spectrum_analysis", tevi_model, data_selection, signal_name, window_range)

  callModule(surface_tension_results_ctrl, "results", tevi_model, sample_specs, parameter_estimates)
}

