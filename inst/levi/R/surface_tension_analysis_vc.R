
surface_tension_analysis_UI <- function(id) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    spectrumUI(ns("spectrum_analysis")),
    surface_tension_results_UI(ns("results"))
  )
}

surface_tension_analysis_ctrl <- function(input, output, session, tevi_model, sample_specs, tasks, notifications){

  data_selection <- reactive({
    tevi_model()$tevi_data %>%
      filter(t %>% between(window_range()[1], window_range()[2]))
  })

  # oscillogram------
  c(signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)
  # periodogram----------
  parameter_estimates <- callModule(spectrum_ctrl, "spectrum_analysis", tevi_model, data_selection, signal_name, window_range)
  # results --------
  callModule(surface_tension_results_ctrl, "results", tevi_model, sample_specs, parameter_estimates)
}

