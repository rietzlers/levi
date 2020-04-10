
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Result-Plots",
        fluidRow(
          column(width = 6,
                 plotlyOutput(ns("surface_tension_plot")),
                 verbatimTextOutput(ns("click_on_st"))),
          column(width = 6, plotlyOutput(ns("viscosity_plot")))
          )
      ),
      tabPanel("Controls",
               actionButton(ns("add_result"), label = "Add current result to result-data", icon = icon("save")),
               fluidRow(
                 column(width = 12, selectInput(ns("st_xvar"), label = "st-time axis", selected = "t", choices = c("t", "smoothed_temp"))),
                 column(width = 12, selectInput(ns("st_yvar"), label = "st ordinate", selected = "f", choices = c("f", "st")))
               )
      )
      ),
      box(width = 12,
          title = "Spectrum-Analysis-Results-Data", collapsible = TRUE, collapsed = TRUE, {
          DT::dataTableOutput(ns("spec_analsis_results_DT"))
      })
    )
}

results_ctrl <- function(input, output, session, tevi_model, sample_specs, live_parameter_estimates){

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
        plot_ly(source = session$ns("st_results")) %>%
        add_trace(
          type = "scatter", mode = "markers",
          x = ~t, y = ~dom_freq_estimate, color = ~calc_method,
          name = "Freq. with max. Amplitude"
        )%>%
        add_trace(data = live_parameter_estimates(),
                  type = "scatter", mode = "markers",
                  x = ~t, y = ~dom_freq_estimate, color = ~calc_method,
                  marker = list(symbol = "square-cross-open", size = 10),
                  name = "Freq. with max. Amplitude"
        ) %>%
        layout(
          legend = list(
            x = 0.8, y = 0.9
          ),
          xaxis = list(
            title = "time [s]"
          ),
          yaxis = list(
            title = "Freq [Hz]",
            range = c(10, 70)
          )
        ) %>%
        event_register("plotly_click")

    })

  output$click_on_st <- renderPrint({
    d <- event_data("plotly_click", source = session$ns("st_results"))
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
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

