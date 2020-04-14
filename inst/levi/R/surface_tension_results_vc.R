
surface_tension_results_UI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 2,
                 box(width = 12,
                 actionButton(ns("add_result"), label = "Save current values", icon = icon("save")),
                 actionButton(ns("show_st_results"), label = "View Results-Table", icon = icon("database")),
                 selectInput(ns("st_xvar"), label = "Display: ", selected = "t", choices = c("time [s]" = "t", "Temp. [° C]" = "smoothed_temp")),
                 selectInput(ns("st_yvar"), label = NULL, selected = "f", choices = c("Freq. [Hz]" = "f","Surface-Tension [Nm]" = "st"))
                 )
          ),
          column(width = 10,
                 plotlyOutput(ns("surface_tension_plot"), height = "350px"))
          )),
      bsModal("st_results_view", "Surface-Tension Results", ns("show_st_results"),
              DT::dataTableOutput(ns("spec_analsis_results_DT")),
              size = "large"
      )
    )
}

surface_tension_results_ctrl <- function(input, output, session, tevi_model, sample_specs, parameter_estimates){

  # data ----------
  live_parameter_estimates <- reactive({
    parameter_estimates() %>%
      mutate(
        temp = convert_to_temp(t, tevi_model()$tevi_data),
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


  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({

    validate(need(spec_analysis_results(), message = "need spec-analysis-results"))

    if(input$st_xvar == "t"){
      x_var <- "t"
      x_range <- range(tevi_model()$tevi_data[["t"]], na.rm = TRUE)
      x_axis_title <- "time [s]"
    }else{
      x_var <- "temp"
      x_range <- range(convert_to_temp(tevi_model()$tevi_data[["t"]], tevi_model()$tevi_data), na.rm = TRUE)
      x_axis_title <- "Temp. [°C]"
    }

    if(input$st_yvar == "f"){
      y_var <- "dom_freq_estimate"
      y_axis_title <- "Dom. Freq. of Signal [Hz]"
      y_range <- c(10, 70)
    }else{
      y_var <- "st"
      y_range <- to_surface_tension(c(10, 70), sample_specs()$mass)
      y_axis_title <- "Surface-Tension of Alloy [Nm]"
    }

    browser()
      spec_analysis_results() %>%
        plot_ly(source = "st_plot") %>%
        add_trace(
          name = "Freq-Estimates",
          type = "scatter",
          mode = "markers",
          x = ~ get(x_var),
          y = ~ get(y_var),
          color = ~ calc_method,
          hovertemplate = "%{y:.1f} Hz"
        ) %>%
        add_trace(
          data = live_parameter_estimates(),
          name = "Freq",
          type = "scatter",
          mode = "markers",
          x = ~ get(x_var),
          y = ~ get(y_var),
          color = ~ calc_method,
          marker = list(symbol = "square-cross-open", size = 10),
          hovertemplate = "%{y:.1f} Hz",
          showlegend = FALSE
        ) %>%
        layout(
          legend = list(x = 0.8, y = 0.9),
          xaxis = list(title = x_axis_title,
                       range = x_range),
          yaxis = list(title = y_axis_title,
                       range = y_range)
        )
    })

  # st-results-table ---------
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

