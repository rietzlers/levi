
surface_tension_result_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12, title = "Surface-Tension Results-Data", collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          column(width = 1,
                 actionButton(ns("delete_rows"), label = "del. rows", icon = icon("minus-square"), width = "100%"),
                 bsTooltip(ns("delete_rows"), "delete selected rows in datatable"),
                 actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
                 bsTooltip(ns("show_ctrls"), "Show additional controls")
          ),
          column(width = 11,
                 DT::dataTableOutput(ns("spec_analsis_results_DT"))
          )
        )
    ),
    bsModal(ns("delete_data_confirmation"), title = "Please confirm deletion", trigger = ns("delete_rows"),
            actionButton(ns("deletion_confirmed"), label = "delete", icon = icon("trash-alt")),
            DT::dataTableOutput(ns("data_selection")),
            size = "large"
    )
  )
}

surface_tension_results_data_ctrl <- function(input, output, session, tevi_model, sample_specs, parameter_estimates, add_result){

  # data ----------
  live_parameter_estimates <- reactive({
    parameter_estimates() %>%
      mutate(
        temp = convert_to_temp(t, tevi_model()$analysis_data),
        st = to_surface_tension(dom_freq_estimate, sample_specs()$mass),
        tevi_data_name = tevi_model()$tevi_data_name
      )
  })

  spec_analysis_results <- reactiveVal(NULL)
  observeEvent(
    add_result(), {
      if(is.null(spec_analysis_results())){
        spec_analysis_results(live_parameter_estimates())
      }else{
        spec_analysis_results(
          dplyr::union(
            spec_analysis_results(),
            live_parameter_estimates()
          )
        )}
    })

  observeEvent(
    input$delete_rows,
    {
      rows <- input$spec_analsis_results_DT_rows_selected
      print(rows)
      output$data_selection <- renderDT(spec_analysis_results()[rows, ])
    }
  )

  observeEvent(
    input$deletion_confirmed,
    {
      rows <- input$spec_analsis_results_DT_rows_selected
      print(rows)
      spec_analysis_results(
        spec_analysis_results()[-rows, ]
      )
      toggleModal(session, "delete_data_confirmation", toggle = "close")
    },
    ignoreInit = TRUE
  )
  # bookmark-callbacks ---------------
  onBookmark(function(state){
    state$values$spec_analysis_results <- spec_analysis_results()
  })
  onRestore(function(state){
    spec_analysis_results(state$values$spec_analysis_results)
  })

  # st-results-table ---------
  output$spec_analsis_results_DT <- DT::renderDataTable({
    validate(need(spec_analysis_results(), label = "spec_analysis_results"))
    spec_analysis_results() %>%
      mutate(
        t = round(t, 3),
        dom_freq_estimate = round(dom_freq_estimate, 2),
        damping_estimate = round(damping_estimate, 1),
        win_length = round(win_end - win_start, 2),
        win_start = round(win_start, 2),
        win_end = round(win_end, 2),
        temp = round(temp, 1),
        st = round(st, 4)
      ) %>%
      select(
        t, dom_freq_estimate, temp, st, damping_estimate, calc_method, everything()
      ) %>%
      mutate(
        calc_method = factor(calc_method),
        signal = factor(signal),
        tevi_data_name = factor(tevi_data_name),
        spans = factor(spans)
      ) %>%
      arrange(t, calc_method) %>%
      datatable(
        rownames = FALSE,
        colnames = c("time [s]" = "t", "Estimate of dom. Freq. [Hz]" = "dom_freq_estimate",
                     "Temp. [° C]" = "temp", "Surface-Tension [Nm]" ="st",
                     "Damping-Estimate [1/s]" = "damping_estimate"),
        filter = 'top',
        extensions = c('Buttons', 'Responsive'),
        options = list(
          #initComplete = JS('function(setting, json) { alert("done"); }'),
          dom = 'Bftli',
          buttons = list(I('colvis'), list(extend = "collection", buttons = c('csv', 'excel', 'pdf'), text = "Download"), 'copy', 'print'),
          paging = FALSE,
          autoWidth = TRUE
        )
      )
  },
  server = TRUE)

  # return-values ----------

  reactive(
    spec_analysis_results()
  )
}
