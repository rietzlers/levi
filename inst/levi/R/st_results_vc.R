st_results_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, plotlyOutput(ns("surface_tension_plot"), height = "400px")),
    box(width = 12,
        fluidRow(
          column(width = 1,
                 actionButton(ns("delete_rows"), label = "del. rows", icon = icon("minus-square"), width = "100%"),
                 bsTooltip(ns("delete_rows"), "delete selected rows in datatable"),
                 actionButton(ns("upload_st_data"), label = "load data", icon = icon("file-upload"),  width = "100%"),
                 bsTooltip(ns("upload_st_data"), "Upload data")
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
        bsModal(ns("upload_st_data_dialog"), trigger = ns("upload_st_data"), size = "large",
                title = "Upload data",
                fileInput(ns("upload_st_data_file"), "Choose ST-data-table to upload (csv-file)",
                          accept = c('text/csv', 'text/comma-separated-values', '.csv'))
        )
    )
  )
}

st_results_ctrl <- function(input, output, session, live_estimates, add_estimate, model, sample_specs, tasks, notifications, selected_sidebar_tab){


  parameter_estimates <- reactiveVal(NULL)
  observeEvent(add_estimate(), {

    if (is.null(parameter_estimates())) {
      updated_results <-
        live_estimates() %>%
        mutate(add_date = Sys.time())
    } else{
      updated_results <-
        dplyr::union(
          parameter_estimates(),
          live_estimates() %>%
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
      select(t, temp, f_0, f_dom, st_0, st_dom, d, lp_limit, hp_limit, win_start, win_end, signal, tevi_data_name,  everything()) %>%
      arrange(t)

    parameter_estimates(updated_results)

  })

  observeEvent(input$delete_rows, {
      rows <- input$spec_analsis_results_DT_rows_selected
      output$data_selection <-
        renderDT(
          parameter_estimates()[rows, ],
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
    })

  observeEvent(input$deletion_confirmed, {
      rows <- input$spec_analsis_results_DT_rows_selected
      parameter_estimates(parameter_estimates()[-rows, ])
      toggleModal(session, "delete_data_confirmation", toggle = "close")
    },
    ignoreInit = TRUE)

  observeEvent(input$upload_st_data_file,{
    upload_file <- input$upload_st_data_file
    tryCatch({
      st_data <-
        readr::read_csv(
          upload_file$datapath
        )
      parameter_estimates(st_data)
    }, error = function(e){
        showModal(modalDialog(title = "Upload-Error",
                              "Uploading data did not succed! Maybe the csv-file is corrupt.
                              Check for standard column-names, seperators ..."))
      })
  }, ignoreInit = TRUE)

  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({
    validate(need(parameter_estimates(), label = "parameter_estimates"))

    parameter_estimates() %>%
      plot_ly(source = "st_plot") %>%
      add_markers(x = ~temp, y = ~ st_0, name = "st_0", hovertemplate = "%{y:.3f} N/m",
                  marker = list(color = "red")) %>%
      add_markers(x = ~ temp, y = ~ st_dom, name = "st_dom", hovertemplate = "%{y:.3f N/m}",
                  marker = list(color = "black")) %>%
      layout(
        #legend = list(x = 0.8, y = 0.9),
        xaxis = list(title = "Temp. [°C]"),
        yaxis = list(title = "Surface-Tension [N/m]")
      )

  })

  # st-results-table ---------
  output$spec_analsis_results_DT <- DT::renderDataTable({
    validate(need(parameter_estimates(), label = "parameter_estimates"))

    parameter_estimates()  %>%
      datatable(
        rownames = FALSE,
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

  # bookmark-callbacks ---------------
  onBookmark(function(state){
    state$values$parameter_estimates <- parameter_estimates()
  })
  onRestore(function(state){
    parameter_estimates(state$values$parameter_estimates)
  })

  # return-values ----------
  return(reactive({parameter_estimates()}))

}
