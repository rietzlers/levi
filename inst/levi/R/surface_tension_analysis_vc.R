
surface_tension_analysis_UI <- function(id) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    spectrumUI(ns("spectrum_analysis")),
    surface_tension_results_UI(ns("st-results")
    )
  )
}

surface_tension_analysis_ctrl <- function(input, output, session, tevi_model, sample_specs, tasks, notifications, selected_sidebar_tab, show_ctrls){

  tapered_data <- reactive({
    tevi_model()$analysis_data %>%
      filter(t %>% between(window_range()[1], window_range()[2]))
  })

  # oscillogram -> c(signal_name, window_range) ------
  c(signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  # spectrum -> parameter_estimates----------
  parameter_estimates <-
    callModule(spectrum_ctrl, "spectrum_analysis", tapered_data, reactive(tevi_model()$frame_rate), signal_name, show_ctrls)

  live_parameter_estimates <-
    reactive({
      parameter_estimates() %>%
        mutate(
          t = mean(window_range()),
          win_start = window_range()[1],
          win_end = window_range()[2],
          temp = convert_to_temp(t, tevi_model()$analysis_data),
          st_dom = to_surface_tension(f_dom, sample_specs()$mass),
          st_0 = to_surface_tension(f_0, sample_specs()$mass),
          tevi_data_name = tevi_model()$tevi_data_name,
          signal = signal_name()
        )
    })

   # st-results -> NULL  --------
  callModule(surface_tension_results_ctrl, "st-results", tevi_model, sample_specs, live_parameter_estimates)
}

