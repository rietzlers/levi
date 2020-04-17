
surface_tension_results_UI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 1,
                 actionButton(ns("add_result"), label = "save", icon = icon("save"), width = "100%"),
                 bsTooltip(ns("add_result"), "add the current values to the result-dataset"),
                 actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
                 bsTooltip(ns("show_ctrls"), "Show additional controls")
          ),
          column(width = 11,
                 plotlyOutput(ns("surface_tension_plot"), height = "350px"))
          )),
      box(width = 12, title = "Surface-Tension Results-Data", collapsible = TRUE, collapsed = TRUE,
          DT::dataTableOutput(ns("spec_analsis_results_DT"))
      ),
    bsModal(ns("additional_ctrls"), title = "Additional Controls for Surface-Tension-Results-Data", trigger = ns("show_ctrls"),
            box(title = "Axis-Scaling of result-plot",
                fluidRow(
                  column(width = 6,
                         selectInput(ns("st_yvar"), label = NULL, selected = "f", choices = c("Freq. [Hz]" = "f","Surface-Tension [Nm]" = "st"))),
                  column(width = 6,
                         selectInput(ns("st_xvar"), label = NULL, selected = "t", choices = c("time [s]" = "t", "Temp. [° C]" = "smoothed_temp"))),
                  tags$div("Time = 0s is different for each tevi-data-set! This means you have to be carful in interpreting the plot.
                           Only temprature allows unambigous interpretation.")
                )
            ),
            size = "large")
    )
}

surface_tension_results_ctrl <- function(input, output, session, tevi_model, sample_specs, parameter_estimates){

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


  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({

    validate(need(spec_analysis_results(), message = "need spec-analysis-results"))

    if(input$st_xvar == "t"){
      x_var <- "t"
      x_range <- range(tevi_model()$analysis_data[["t"]], na.rm = TRUE)
      x_axis_title <- "time [s]"
    }else{
      x_var <- "temp"
      x_range <- range(convert_to_temp(tevi_model()$analysis_data[["t"]], tevi_model()$analysis_data), na.rm = TRUE)
      x_axis_title <- "Temp. [°C]"
    }

    if(input$st_yvar == "f"){
      y_var <- "dom_freq_estimate"
      y_axis_title <- "Dom. Freq. of Signal [Hz]"
      y_range <- c(10, 70)
      y_unit <- "Hz"
    }else{
      y_var <- "st"
      y_range <- to_surface_tension(c(10, 70), sample_specs()$mass)
      y_axis_title <- "Surface-Tension of Alloy [Nm]"
      y_unit <- "Nm"
    }

    calc_method_trace <- function(p, cm, trace,  estimates = NULL, p_size = 6,  sl = TRUE, fit_lm = FALSE, farbe = "black"){
      if(missing(trace)){trace = cm}
      p <-
        p %>%
        filter(calc_method == cm) %>%
        add_trace(
          data = estimates,
          name = trace,
          type = "scatter",
          mode = "markers",
          x = ~ get(x_var),
          y = ~ get(y_var),
          hovertemplate = paste("%{y:.2f}", y_unit),
          size = I(p_size),
          showlegend = sl,
          color = I(farbe)
        )
      if(fit_lm == TRUE){
        complete_obs <-
          plotly_data(p) %>%
          filter(!is.na(dom_freq_estimate)) %>%
          filter(calc_method == cm)
        lm <- lm(get(y_var) ~ get(x_var), data = complete_obs)
        c(y0, m) %<-% coef(lm)

        lm_data <-
          tibble(
            x = complete_obs[[x_var]],
            fitted_values = predict(lm)
          )
        p <-
          p %>%
          add_lines(
            data = lm_data,
            name = str_glue("{round(y0, 2)} + {round(m, 4)} x"),
            x = ~x, y = ~fitted_values,
            hoverinfo = "none",
            color = I(farbe)
          )
      }
      p
    }

      spec_analysis_results() %>%
        plot_ly(source = "st_plot") %>%
        add_fun(function(p){calc_method_trace(p, "fft", fit_lm = TRUE, farbe = "blue")}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum", fit_lm = TRUE, farbe = "green")}) %>%
        add_fun(function(p){calc_method_trace(p, "fft_lorentz", fit_lm = TRUE, farbe ="yellow")}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum_lorentz", fit_lm = TRUE, farbe = "orange")}) %>%
        add_fun(function(p){calc_method_trace(p, "fft", trace = "current estimate", p_size = 20,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum",  trace = "current estimate", p_size = 20,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "fft_lorentz",  trace = "current estimate", p_size = 20,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum_lorentz",  trace = "current estimate", p_size = 20,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
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

