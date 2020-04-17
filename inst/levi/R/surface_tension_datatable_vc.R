
surface_tension_datatable_UI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("add_result"), label = "save", icon = icon("save"), inline = TRUE),
    bsTooltip(ns("add_result"), "add the current values to the result-dataset"),
    actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  inline = TRUE),
    bsTooltip(ns("show_ctrls"), "Show additional controls"),
    bsModal(ns("additional_ctrls"), title = "Additional Controls", trigger = ns("show_ctrls"),
            size = "large")
  )

}

surface_tension_datatable_ctrl <- function(input, output, session, tevi_model, sample_specs, parameter_estimates){

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
  observeEvent(input$add_result, {
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

}
