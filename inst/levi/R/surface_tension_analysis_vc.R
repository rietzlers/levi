
surface_tension_analysis_UI <- function(id) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    spectrumUI(ns("spectrum_analysis")),
    surface_tension_results_UI(ns("st-results")
    )
  )
}

surface_tension_analysis_ctrl <- function(input, output, session, tevi_model, sample_specs, tasks, notifications, selected_sidebar_tab){

  tapered_data <- reactive({
    tevi_model()$analysis_data %>%
      filter(t %>% between(window_range()[1], window_range()[2]))
  })

  # oscillogram------
  c(signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  # periodogram----------
  parameter_estimates <-
    callModule(
      spectrum_ctrl, "spectrum_analysis",
      tapered_data, reactive(tevi_model()$frame_rate), signal_name, window_range
    )

   # st-results  --------
  callModule(surface_tension_results_ctrl, "st-results", tevi_model, sample_specs, parameter_estimates)
}

