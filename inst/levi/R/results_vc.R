
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
        div(
          plotlyOutput(ns("surface_tension_plot")),
          fluidRow(
            column(width = 6, selectInput(ns("st_xvar"), label = "", selected = "t", choices = c("t", "smoothed_temp"))),
            column(width = 6, selectInput(ns("st_yvar"), label = "", selected = "f", choices = c("f", "st")))
          )
        )
      ),
      column(width = 6,
        div(
          plotlyOutput(ns("viscosity_plot")),
          fluidRow(
            column(width = 6, selectInput(ns("visc_xvar"), label = "",selected = "t", choices = c("t", "smoothed_temp"))),
            column(width = 6, selectInput(ns("visc_yvar"), label = "", selected = "d", choices = c("d", "viscosity")))
          )
        )
      ),
      box(width = 12,
          title = "Table of Spectrum-Annalysis-Results", collapsible = TRUE, collapsed = TRUE, {
          DT::dataTableOutput(ns("spec_analsis_results_DT"))
      })
    )
  )
}


results_ctrl <-
  function(input, output, session,
           tevi_model, sample_specs, data_selection, time_range, signal_name,
           type, bp, dom_freq, f0, d, spans, taper,
           selected_tab, spectrum_results_UI){
    # data ----------
    spec_analysis_results <- reactiveVal()
    observeEvent(input$add_result, {
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
    # UI -----------
    observeEvent(selected_tab(),{
      if (selected_tab() == "signalAnalysis"){
        spectrum_results_UI(
          box(width = 12, title = "Spec/FFT-Results", collapsible = TRUE, collapsed = TRUE,
              actionButton(session$ns("add_result"), label = "add result", icon = icon("save"))
          ))
      }else{
        spectrum_results_UI(NULL)
      }
    })
    # output-ctrls -----------
    output$spec_analsis_results_DT <- DT::renderDataTable({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>% arrange(type, t)
    },
    server = TRUE, filter = 'top',
    extensions = c('Buttons', 'Responsive'),
    options = list(
      dom = 'Bftlip',
      buttons = c('csv', 'excel', 'pdf'),
      pageLength = 5,
      autoWidth = TRUE
    ))
  output$surface_tension_plot <- renderPlotly({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))

      if(input$st_yvar == "st"){
        ylim <- to_surface_tension(bp(), sample_specs()$mass)
      }else{
        ylim <- bp()
      }
      sf_plot <-
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
        theme(legend.position="none") +
        ylim(ylim)

      sf_plot <-
        sf_plot +
        geom_point(aes(x = mean(time_range()) %>% round(2), y = dom_freq()), size = 2, color = "blue")
      sf_plot
    })

  output$viscosity_plot <- renderPlotly({
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

