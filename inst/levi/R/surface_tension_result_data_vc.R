
surface_tension_result_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 1,
             actionButton(ns("delete_rows"), label = "del. rows", icon = icon("minus-square"), width = "100%"),
             bsTooltip(ns("delete_rows"), "delete selected rows in datatable"),
             actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
             bsTooltip(ns("show_ctrls"), "Show additional controls")
      ),
      column(width = 11,
             DT::dataTableOutput(ns("spec_analsis_results_DT"), height = "400px")
      )
    ),
    bsModal(ns("delete_data_confirmation"), trigger = ns("delete_rows"), size = "large",
            title = tagList("Delete selected observations?",
                            actionButton(ns("deletion_confirmed"), label = "delete", icon = icon("trash-alt"))),
            DT::dataTableOutput(ns("data_selection"))
            ),
    bsModal(ns("ctrls"), trigger = ns("show_ctrls"), size = "large",
            title = "Additiona controls for data-table",
            fileInput(ns("upload_st_data"), "Choose ST-data-table to upload (csv-file)",
                      accept = c('text/csv', 'text/comma-separated-values', '.csv'))
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
        updated_results <-
          live_parameter_estimates() %>%
          mutate(
            win_length = round(win_end - win_start, 2),
            add_date = Sys.time()
            )
      }else{
        updated_results <-
          dplyr::union(
            spec_analysis_results(),
            live_parameter_estimates() %>%
              mutate(
                win_length = round(win_end - win_start, 2),
                add_date = Sys.time()
              )
          )
      }

      updated_results <-
        updated_results %>%
        mutate(
          t = round(t, 3),
          dom_freq_estimate = round(dom_freq_estimate, 2),
          damping_estimate = round(damping_estimate, 1),
          win_start = round(win_start, 2),
          win_end = round(win_end, 2),
          temp = round(temp, 1),
          st = round(st, 4)
        ) %>%
        mutate(
          calc_method = factor(calc_method),
          signal = factor(signal),
          tevi_data_name = factor(tevi_data_name),
          spans = factor(spans)
        ) %>%
        select(
          t, dom_freq_estimate, temp, st, damping_estimate, calc_method, everything()
        ) %>%
        arrange(t, calc_method)

      spec_analysis_results(updated_results)
    })

  observeEvent(
    input$delete_rows,
    {
      rows <- input$spec_analsis_results_DT_rows_selected
      output$data_selection <-
        renderDT(
          spec_analysis_results()[rows, ],
          rownames = FALSE,
          filter = 'top',
          extensions = c('Buttons', 'Responsive'),
          options = list(
            #initComplete = JS('function(setting, json) { alert("done"); }'),
            dom = 'Bftli',
            buttons = list(I('colvis'), list(extend = "collection", buttons = c('csv', 'excel', 'pdf'), text = "Download"), 'copy', 'print'),
            paging = TRUE,
            autoWidth = TRUE
          ))
    }
  )

  observeEvent(
    input$deletion_confirmed,
    {
      rows <- input$spec_analsis_results_DT_rows_selected
      spec_analysis_results(spec_analysis_results()[-rows, ])
      toggleModal(session, "delete_data_confirmation", toggle = "close")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$upload_st_data,{
    upload_file <- input$upload_st_data
    tryCatch(
      error = function(e){
        showModal(modalDialog(title = "Upload-Error",
                              "Uploading data did not succed! Maybe the csv-file is corrupt.
                              Check for standard column-names, seperators ..."))
      },
      {
      st_data <-
        readr::read_csv(
          upload_file$datapath,
          col_types =
            cols(
              `time [s]` = col_double(),
              `Estimate of dom. Freq. [Hz]` = col_double(),
              `Temp. [° C]` = col_double(),
              `Surface-Tension [Nm]` = col_double(),
              `Damping-Estimate [1/s]` = col_double(),
              calc_method = col_character(),
              spans = col_character(),
              taper = col_double(),
              hp_limit = col_double(),
              lp_limit = col_double(),
              win_start = col_double(),
              win_end = col_double(),
              signal = col_character(),
              tevi_data_name = col_character(),
              win_length = col_double(),
              add_date = col_datetime(format = "")
            )
        ) %>%
        dplyr::rename(
          t = "time [s]",
          dom_freq_estimate = "Estimate of dom. Freq. [Hz]",
          temp = "Temp. [° C]",
          st = "Surface-Tension [Nm]",
          damping_estimate = "Damping-Estimate [1/s]"
        )
      spec_analysis_results(st_data)
    }
    )
  }, ignoreInit = TRUE)

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
    spec_analysis_results()  %>%
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
