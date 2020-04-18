
surface_tension_results_UI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 1,
                 actionButton(ns("add_result"), label = "save", icon = icon("save"), width = "100%"),
                 bsTooltip(ns("add_result"), "add the current values to the result-dataset"),
                 selectInput(ns("selected_view"), label = "show", choices = c("ST-Plot" = "st_plot", "ST-Datatable" = "st_datatable")),
                 actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
                 bsTooltip(ns("show_ctrls"), "Show additional controls")
          ),
          column(width = 11,
                 tags$style(paste("#", ns("tab_wizard"), " { display:none; }", sep = "")),
                 tabsetPanel(id = ns("tab_wizard"),
                             tabPanel(ns("st_plot"),
                                      plotlyOutput(ns("surface_tension_plot"), height = "400px")
                                      ),
                             tabPanel(ns("st_datatable"),
                                      surface_tension_result_data_UI(ns("st_result_data"))
                                      )
                            )
                 )
          )
        ),
    bsModal(ns("additional_ctrls"), title = "Additional Controls for Surface-Tension-Results-Data", trigger = ns("show_ctrls"),
            box(title = "Axis-Scaling of result-plot",
                fluidRow(
                  column(width = 6,
                         selectInput(ns("st_yvar"), label = NULL, selected = "f", choices = c("Freq. [Hz]" = "f","Surface-Tension [Nm]" = "st"))),
                  column(width = 6,
                         selectInput(ns("st_xvar"), label = NULL, selected = "t", choices = c("time [s]" = "t", "Temp. [° C]" = "smoothed_temp"))),
                  tags$div("Time = 0s is different for each tevi-data-set! This means you have to be carful in interpreting the plot.
                           Only temperature allows unambigous interpretation.")
                )
            ),
            size = "large")
    )
}

surface_tension_results_ctrl <- function(input, output, session, tevi_model, sample_specs, parameter_estimates){

  live_parameter_estimates <- reactive({
    parameter_estimates() %>%
      mutate(
        temp = convert_to_temp(t, tevi_model()$analysis_data),
        st = to_surface_tension(dom_freq_estimate, sample_specs()$mass),
        tevi_data_name = tevi_model()$tevi_data_name
      )
  })

  observe(
    updateTabsetPanel(session, "tab_wizard", selected = session$ns(input$selected_view))
  )

  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({

    validate(need(spec_analysis_results(), message = "need spec-analysis-results"))

    if(input$st_xvar == "t"){
      x_var <- "t"
      x_axis_title <- "time [s]"
    }else{
      x_var <- "temp"
      x_axis_title <- "Temp. [°C]"
    }

    if(input$st_yvar == "f"){
      y_var <- "dom_freq_estimate"
      y_axis_title <- "Dom. Freq. of Signal [Hz]"
      y_unit <- "Hz"
    }else{
      y_var <- "st"
      y_axis_title <- "Surface-Tension of Alloy [Nm]"
      y_unit <- "Nm"
    }

    calc_method_trace <- function(p, cm, trace,  estimates = NULL, p_size = 15,  sl = TRUE, fit_lm = FALSE, farbe = "red"){
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
        add_fun(function(p){calc_method_trace(p, "fft_lorentz", fit_lm = TRUE, farbe ="darkblue")}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum", fit_lm = TRUE, farbe = "black")}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum_lorentz", fit_lm = TRUE, farbe = "darkgrey")}) %>%
        add_fun(function(p){calc_method_trace(p, "fft", trace = "current estimate", p_size = 30,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum",  trace = "current estimate", p_size = 30,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "fft_lorentz",  trace = "current estimate", p_size = 30,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        add_fun(function(p){calc_method_trace(p, "spectrum_lorentz",  trace = "current estimate", p_size = 30,
                                              estimates = live_parameter_estimates(), sl = FALSE)}) %>%
        layout(
          legend = list(x = 0.8, y = 0.9),
          xaxis = list(title = x_axis_title),
          yaxis = list(title = y_axis_title)
        )

    })

  spec_analysis_results <- callModule(surface_tension_results_data_ctrl, "st_result_data", tevi_model, sample_specs, parameter_estimates,
                                      reactive(input$add_result))

  # return-values ----------

}

