st_results_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, plotlyOutput(ns("surface_tension_plot"), height = "400px")),
    box(width = 12, surface_tension_result_data_UI(ns("st_result_data")))
  )
}

st_results_ctrl <- function(input, output, session, tevi_model, sample_specs, live_parameter_estimates, add_result){

  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({

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

  spec_analysis_results <-
    callModule(surface_tension_results_data_ctrl, "st_result_data", tevi_model, sample_specs, live_parameter_estimates, add_result)

  # return-values ----------

}
