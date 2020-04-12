
surface_tension_results_UI <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
          column(width = 2,
                 actionButton(ns("add_result"), label = "Add current result to result-data", icon = icon("save")),
                 selectInput(ns("st_xvar"), label = "st-time axis", selected = "t", choices = c("t", "smoothed_temp")),
                 selectInput(ns("st_yvar"), label = "st ordinate", selected = "f", choices = c("f", "st")),
                 actionButton(ns("show_st_results"), label = "View Results-Table", icon = icon("database"))),
          column(width = 10,
                 plotlyOutput(ns("surface_tension_plot")))
          ),
      bsModal("st_results_view", "Surface-Tension Results", ns("show_st_results"),
              DT::dataTableOutput(ns("spec_analsis_results_DT")),
              size = "large"
      )
    )
}

surface_tension_results_ctrl <- function(input, output, session, tevi_model, sample_specs, live_parameter_estimates){

  # data ----------
  spec_analysis_results <- reactiveVal(
    bind_cols(
      c("t", "dom_freq_estimate", "damping_estimate", "lp_limit", "hp_limit", "win_start", "win_end", "taper") %>% purrr::map_dfc(setNames, object = list(numeric())),
      c("signal", "calc_method", "spans", "tevi_data_name") %>% purrr::map_dfc(setNames, object = list(character()))
    ),
    label = "spec_analysis_results"
  )

  # observers ---------
  observeEvent(input$add_result, {
    spec_analysis_results(
      spec_analysis_results() %>%
        dplyr::union(live_parameter_estimates() %>% mutate(tevi_data_name = tevi_model()$tevi_data_name))
      )
  }) # add temp result to result-data

  # bookmark-callbacks ---------------
  onBookmark(function(state){
    state$values$spec_analysis_results <- spec_analysis_results()
  })
  onRestore(function(state){
    spec_analysis_results(state$values$spec_analysis_results)
  })

  # plot-outputs  -----------
  output$surface_tension_plot <- renderPlotly({

      validate(need(spec_analysis_results(), label = "spec_analysis_results"))

      spec_analysis_results() %>%
        plot_ly(source = session$ns("surface_tension_results")) %>%
        add_trace(name = "Freq-Estimates", type = "scatter", mode = "markers",
          x = ~t, y = ~dom_freq_estimate, color = ~calc_method,
          hovertemplate = "%{y:.1f} Hz"
        )%>%
        add_trace(data = live_parameter_estimates(),
                  name = "Freq", type = "scatter", mode = "markers",
                  x = ~t, y = ~dom_freq_estimate, color = ~calc_method,
                  marker = list(symbol = "square-cross-open", size = 10),
                  hovertemplate = "%{y:.1f} Hz",
                  showlegend = FALSE

        ) %>%
        layout(
          legend = list(
            x = 0.8, y = 0.9
          ),
          xaxis = list(
            title = "time [s]",
            range = range(tevi_model()$tevi_data[["t"]])
          ),
          yaxis = list(
            title = "Freq [Hz]",
            range = c(10, 70)
          )
        )

    })


  # results-table-output ---------
  output$spec_analsis_results_DT <- DT::renderDataTable({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        mutate(t = round(t, 2)) %>%
        arrange(calc_method, t)
    },
    rownames = FALSE,
    server = TRUE, filter = 'top',
    extensions = c('Buttons', 'Responsive', 'FixedColumns'),
    options = list(
      dom = 'Bftlip',
      buttons = c('csv', 'excel', 'pdf', I('colvis')),
      pageLength = 5,
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 1)
    ))

  # return-values ----------

}

