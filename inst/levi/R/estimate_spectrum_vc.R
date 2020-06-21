
estimate_spectrum_UI <- function(id) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    spectrumUI(ns("spectrum_analysis")),
    fluidRow(column(width = 12, plotlyOutput(ns("freq_vs_time_plot"), height = "400px")))
  )
}

estimate_spectrum_ctrl <- function(input, output, session, tevi_model, sample_specs, tasks, notifications, selected_sidebar_tab){

  # local data ---------
  tapered_data <- reactive({
    tevi_model()$analysis_data %>%
      filter(t %>% between(window_range()[1], window_range()[2]))
  })

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

  spec_analysis_results <- reactiveVal(NULL)
  observeEvent(add_result(), {
    if (is.null(spec_analysis_results())) {
      updated_results <-
        live_parameter_estimates() %>%
        mutate(add_date = Sys.time())
    } else{
      updated_results <-
        dplyr::union(
          spec_analysis_results(),
          live_parameter_estimates() %>%
            mutate(add_date = Sys.time())
        )
    }

    updated_results <-
      updated_results %>%
      mutate(
        signal = factor(signal),
        tevi_data_name = factor(tevi_data_name),
        spans = factor(spans)
      ) %>%
      select(t, temp, f_0, f_dom, st_0, st_dom, d, everything()) %>%
      arrange(t)

    spec_analysis_results(updated_results)
  })

  # oscillogram -> c(signal_name, window_range) ------
  c(signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  # spectrum -> c(parameter_estimates, add_result) ----------
  c(parameter_estimates, add_result) %<-%
    callModule(spectrum_ctrl, "spectrum_analysis", tapered_data, reactive(tevi_model()$frame_rate), signal_name)

  # st-result-plot  -----------
  output$freq_vs_time_plot <- renderPlotly({

    validate(need(spec_analysis_results(), message = "need spec-analysis-results"))

    spec_analysis_results() %>%
      plot_ly(source = "st_plot") %>%
      add_markers(x = ~ t, y = ~ f_0, name = "f_0", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "red")) %>%
      add_markers(x = ~ t, y = ~ f_dom, name = "f_dom", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "black")) %>%
      add_markers(data = live_parameter_estimates(), name = "f_0 (current)",
                  x = ~ t, y = ~ f_0, name = "f_0", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "red", symbol = c("x"), size = 10, opacity = 0.5),
                  showlegend = FALSE) %>%
      add_markers(data = live_parameter_estimates(), name = "f_dom (current)",
                  x = ~ t, y = ~ f_dom, name = "f_dom", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "black", symbol = c("x"), size = 10, opacity = 0.5),
                  showlegend = FALSE) %>%
      layout(
        legend = list(x = 0.8, y = 0.9),
        xaxis = list(title = "time [s]"),
        yaxis = list(title = "Freq [Hz]")
      )

  })
}
