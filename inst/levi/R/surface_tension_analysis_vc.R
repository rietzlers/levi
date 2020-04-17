
surface_tension_analysis_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(paste("#", ns("tab_wizard"), " { display:none; }", sep = "")),
    tabsetPanel(id = ns("tab_wizard"),
                tabPanel(ns("st_analysis"),
                         oscillogramUI(ns("oscillogram")),
                         spectrumUI(ns("spectrum_analysis")),
                         surface_tension_results_UI(ns("st-plot"))
                         ),
                tabPanel(ns("st_datatable"),
                         surface_tension_datatable_UI(ns("st-datatable"))
                         )
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
  parameter_estimates <- callModule(spectrum_ctrl, "spectrum_analysis", tevi_model, tapered_data, signal_name, window_range)

  observe(updateTabsetPanel(session, "tab_wizard", selected = session$ns(selected_sidebar_tab())))
   # st-plot  --------
  callModule(surface_tension_results_ctrl, "st-plot", tevi_model, sample_specs, parameter_estimates)

  # st-datatable
  callModule(surface_tension_datatable_ctrl, "results", tevi_model, sample_specs, parameter_estimates)
}

