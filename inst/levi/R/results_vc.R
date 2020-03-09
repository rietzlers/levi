
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
      box(width = 12, {
        DT::dataTableOutput(ns("spec_analsis_results_DT"))
        },
        title = "Table of Spectrum-Annalysis-Results",
        collapsible = TRUE, collapsed = TRUE),
      box(width = 6, {
        div(
          plotlyOutput(ns("surface_tension_plot")),
          fluidRow(
            column(width = 6, selectInput(ns("st_xvar"), label = "", selected = "t", choices = c("t", "smoothed_temp"))),
            column(width = 6, selectInput(ns("st_yvar"), label = "", selected = "f", choices = c("f", "st")))
          )
        )
      },
        title = "Surface-Tension/Frequency-Plot",
        collapsible = TRUE, collapsed = TRUE),
      box(width = 6, {
        div(
          plotlyOutput(ns("viscosity_plot")),
          fluidRow(
            column(width = 6, selectInput(ns("visc_xvar"), label = "",selected = "t", choices = c("t", "smoothed_temp"))),
            column(width = 6, selectInput(ns("visc_yvar"), label = "", selected = "d", choices = c("d", "viscosity")))
          )
        )
      },
        title = "Viscosity/Damping-Plot",
        collapsible = TRUE, collapsed = TRUE)
  )
}


results_ctrl <-
  function(input, output, session,
           tevi_model, sample_specs, data_selection, signal_brush, signal_name,
           type, bp, dom_freq, f0, d, spans, taper, add_result){
    # data ----------
    time_range <- reactive({get_brush_range(signal_brush())})
    spec_analysis_results <- reactiveVal()
    observeEvent(add_result(), {
      if (is.null(spec_analysis_results())) {
        spec_analysis_results(
          tibble(
            type = type(),
            t = mean(time_range()) %>% round(2),
            wl = (time_range()[2] - time_range()[1]) %>% round(2),
            dom_freq = dom_freq(),
            f0 = f0(),
            d = d(),
            spans = spans(),
            taper = taper(),
            signal = signal_name(),
            data = tevi_model()$tevi_data_name
          )
        )
      }
      else{
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
                taper = taper(),
                signal = signal_name(),
                data = tevi_model()$tevi_data_name
              )
            ))
      }
    })
    # output-ctrls -----------
    output$spec_analsis_results_DT <-
      DT::renderDataTable({
        validate(need(spec_analysis_results(), label = "spec_analysis_results"))
        spec_analysis_results() %>% arrange(type, t)
        },
      server = TRUE,
      filter = 'top',
      extensions = c('Buttons'),
      options = list(
        dom = 'lftipB',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5, autoWidth = TRUE)
      )

  output$surface_tension_plot <-
    renderPlotly({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        mutate(
          smoothed_temp = convert_to_temp(t, tevi_model()$tevi_data),
          f_dom_freq = dom_freq,
          f_f0 = f0,
          st_dom_freq = to_surface_tension(dom_freq, sample_specs()$mass),
          st_f0 = to_surface_tension(f0, sample_specs()$mass),
        ) %>%
        ggplot(aes(x = .data[[input$st_xvar]], color = type)) +
        geom_point(aes(y = .data[[paste0(input$st_yvar, "_dom_freq")]]), shape = "x", size = 2) +
        geom_point(aes(y = .data[[paste0(input$st_yvar,"_f0")]]), size = 2) +
        labs(x = "time/Temp", y = "Freqs/Surface-Tension") +
        theme(legend.position="none")
    })

  output$viscosity_plot <-
    renderPlotly({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        mutate(
          smoothed_temp = convert_to_temp(t, tevi_model()$tevi_data),
          viscosity = to_viscosity(d, sample_specs()$mass, sample_specs()$radius),
        ) %>%
        ggplot(aes(x = .data[[input$visc_xvar]], color = type)) +
        geom_point(aes(y = .data[[input$visc_yvar]]),  size = 2) +
        labs(x = "time/temp", y = "Damping/Viscosity") +
        theme(legend.position="none")
    })
}

