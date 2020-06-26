
estimate_spectrum_UI <- function(id) {
  ns <- NS(id)
  tagList(
    oscillogramUI(ns("oscillogram")),
    spectrumUI(ns("spectrum_analysis")),
    fluidRow(column(width = 12, plotlyOutput(ns("freq_vs_time_plot"), height = "400px")))
  )
}

estimate_spectrum_ctrl <- function(input, output, session, parameter_estimates, tevi_model, sample_specs, tasks, notifications, selected_sidebar_tab){

  # local data ---------
  tapered_data <- reactive({
    tevi_model()$analysis_data %>%
      filter(t %>% between(window_range()[1], window_range()[2]))
  })

  live_parameter_estimates <-
    reactive({
      validate(need(sample_specs(), message = "Alloy/Sample-Specifications are missing. Set them in the Dashboard"))
      freq_estimates() %>%
        mutate(
          t = mean(window_range()) %>% round(3),
          win_start = window_range()[1] %>% round(3),
          win_end = window_range()[2] %>% round(3),
          temp = convert_to_temp(t, tevi_model()$analysis_data),
          st_dom = to_surface_tension(f_dom, sample_specs()$mass) %>% round(3),
          st_0 = to_surface_tension(f_0, sample_specs()$mass) %>% round(3),
          tevi_data_name = tevi_model()$tevi_data_name,
          signal = signal_name()
        )
    })

  # oscillogram -> c(signal_name, window_range) ------
  c(signal_name, window_range) %<-% callModule(oscillogram_ctrl, "oscillogram", tevi_model)

  # spectrum -> c(freq_estimates, add_estimate) ----------
  c(freq_estimates, add_estimate) %<-%
    callModule(spectrum_ctrl, "spectrum_analysis", tapered_data, reactive(tevi_model()$frame_rate), signal_name)

  # freq-vs-time-plot  -----------
  output$freq_vs_time_plot <- renderPlotly({

  st_plot <-
    live_parameter_estimates() %>%
      plot_ly(source = "st_plot") %>%
      add_markers(name = "f_0 (current)",
                  x = ~ t, y = ~ f_0, name = "f_0 (live)", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "red", symbol = c("x"), size = 10, opacity = 0.5)) %>%
      add_markers(name = "f_dom (current)",
                  x = ~ t, y = ~ f_dom, name = "f_dom", hovertemplate = "%{y:.1f}  Hz",
                  marker = list(color = "black", symbol = c("x"), size = 10, opacity = 0.5)) %>%
      layout(
        legend = list(x = -0.1, y = 0.9),
        xaxis = list(title = "time [s]", range = range(tevi_model()$analysis_data[["t"]], nar.rm = TRUE)),
        yaxis = list(title = "Freq [Hz]")
      )

  if (!is.null(parameter_estimates())) {
   # lm_f_0 <- lm(f_0 ~ t, data = freq_estimates())
    st_plot <-
      st_plot %>%
      add_markers(
        data = parameter_estimates(),
        x = ~ t,
        y = ~ f_0,
        name = "f_0",
        hovertemplate = "%{y:.1f}  Hz",
        marker = list(color = "red")
      ) %>%
      add_markers(
        data = parameter_estimates(),
        x = ~ t,
        y = ~ f_dom,
        name = "f_dom",
        hovertemplate = "%{y:.1f}  Hz",
        marker = list(color = "black")
      )
  }
  st_plot
  })

  # return-value ---------

  return(
    list(
    reactive({
      validate(need(live_parameter_estimates(), "no live estimates"))
      live_parameter_estimates()
      }),
    add_estimate
    )
  )
}
