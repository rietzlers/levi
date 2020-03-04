
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
      box(width = 12,
        DT::dataTableOutput(ns("spec_analsis_results_DT")),
        title = "Table of Spectrum-Annalysis-Results",
        collapsible = TRUE,
        collapsed = FALSE
      ),
      box(width = 12,
        plotlyOutput(ns("surface_tension_plot")),
        title = "Surface-Tension/Frequency-Plot",
        collapsible = TRUE,
        collapsed = TRUE
        ),
      box(width = 12,
        plotlyOutput(ns("viscosity_plot")),
        title = "Viscosity/Damping-Plot",
        collapsible = TRUE,
        collapsed = TRUE
      )
  )
}


results_ctrl <-
  function(input, output, session, signal_brush, type, bp, dom_freq, f0, d, spans, taper, add_result){
    # data ----------
    time_range <- reactive({get_brush_range(signal_brush())})
    spec_analysis_results <- reactiveVal()
    observeEvent(add_result(),
                    {
                      if(is.null(spec_analysis_results())){
                        spec_analysis_results(
                          tibble(
                          type = type(),
                          t = mean(time_range()) %>% round(2),
                          wl = (time_range()[2] - time_range()[1]) %>% round(2),
                          dom_freq = dom_freq(),
                          f0 = f0(),
                          d = d(),
                          spans = spans(),
                          taper = taper()
                          )
                        )
                      }else{
                        spec_analysis_results(
                          spec_analysis_results() %>%
                          dplyr::union(
                            tibble(
                              type = type(),
                              t = mean(time_range()) %>% round(2),
                              wl = (time_range()[2] - time_range()[1]) %>% round(2),
                              dom_freq = dom_freq(),
                              f0 = f0(),
                              d = d(),
                              spans = spans(),
                              taper = taper()
                            )
                          )
                        )
                      }

                    })

    # output-ctrls -----------
    output$spec_analsis_results_DT <- DT::renderDataTable(
        {
        validate(need(spec_analysis_results(), label = "spec_analysis_results"))
        spec_analysis_results() %>% arrange(type, t)
        },
      server = TRUE,
      filter = 'top',
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5, autoWidth = TRUE)
      )

  # output$spec_analysis_results_display <-
  #   renderPlotly({
  #     validate(need(spec_analysis_results(), label = "spec_analysis_results"))
  #     spec_analysis_results() %>%
  #       ggplot(aes(x = t, color = type)) +
  #       geom_point(aes(y = dom_freq), shape = "x", size = 2) +
  #       geom_point(aes(y = f0), size = 2) +
  #       ylim(bp())
  #   })
}

