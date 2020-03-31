
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Result-Plots",
        fluidRow(
          column(width = 6,plotlyOutput(ns("surface_tension_plot"))),
          column(width = 6, plotlyOutput(ns("viscosity_plot")))
          )
      ),
      tabPanel("Controls",
               fluidRow(
                 column(width = 6, selectInput(ns("st_xvar"), label = "st-time axis", selected = "t", choices = c("t", "smoothed_temp"))),
                 column(width = 6, selectInput(ns("st_yvar"), label = "st ordinate", selected = "f", choices = c("f", "st")))
               ),
               fluidRow(
                 column(width = 6, selectInput(ns("visc_xvar"), label = "viscosity-time axis",selected = "t", choices = c("t", "smoothed_temp"))),
                 column(width = 6, selectInput(ns("visc_yvar"), label = "viscosity ordinate", selected = "d", choices = c("d", "viscosity")))
               )
      )
      ),
      box(width = 12,
          title = "Spectrum-Analysis-Results-Data", collapsible = TRUE, collapsed = TRUE, {
          DT::dataTableOutput(ns("spec_analsis_results_DT"))
      })
    )
}

results_ctrl <- function(input, output, session,
                         tevi_model, sample_specs, data_selection, time_range, signal_name,
                         type, bp, dom_freq, f0, d, spans, taper,
                         selected_tab, spectrum_results_UI){
  # data ----------
  results_data_template <- {
      bind_cols(
      c("t", "wl", "dom_freq", "f0", "d", "taper") %>% purrr::map_dfc(setNames, object = list(numeric())),
      c("signal", "data", "type", "spans") %>% purrr::map_dfc(setNames, object = list(character())),
      c("ephemeral") %>% purrr::map_dfc(setNames, object = list(logical()))
    )}
  spec_analysis_results <- reactiveVal(results_data_template)
  ephemeral_result <- reactive({
    tibble(
      type = type(),
      t = mean(time_range()),
      wl = (time_range()[2] - time_range()[1]),
      dom_freq = dom_freq(),
      f0 = f0(),
      d = d(),
      spans = spans(),
      taper = taper(),
      signal = signal_name(),
      data = tevi_model()$tevi_data_name,
      ephemeral = TRUE
    )
  })
  add_result <- function(result) {
    spec_analysis_results(spec_analysis_results() %>%
                            dplyr::union(result %>% mutate(ephemeral = FALSE)))
  }


  # observers ---------
  observeEvent(input$add_result, {
    add_result(ephemeral_result())
  }) # add temp result to result-data
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

  # bookmark-callbacks ---------------
  onBookmark(function(state){
    state$values$spec_analysis_results <- spec_analysis_results()
  })

  onRestore(function(state){
    spec_analysis_results(state$values$spec_analysis_results)
  })

  # plot-outputs  -----------
  output$surface_tension_plot <- renderPlotly({
      spec_analysis_results <-
        spec_analysis_results() %>% dplyr::union(ephemeral_result())

      validate(need(spec_analysis_results, label = "spec_analysis_results"))

      if(input$st_yvar == "st"){
        ylim <- to_surface_tension(bp(), sample_specs()$mass)
      }else{
        ylim <- bp()
      }
      sf_plot <-
        spec_analysis_results %>%
        mutate(
          smoothed_temp = convert_to_temp(t, tevi_model()$tevi_data),
          f_dom_freq = dom_freq,
          f_f0 = f0,
          st_dom_freq = to_surface_tension(dom_freq, sample_specs()$mass),
          st_f0 = to_surface_tension(f0, sample_specs()$mass),
        ) %>%
        ggplot(aes(x = .data[[input$st_xvar]], color = ephemeral)) +
        geom_point(aes(y = .data[[paste0(input$st_yvar, "_dom_freq")]]), shape = "x", size = 2) +
        geom_point(aes(y = .data[[paste0(input$st_yvar,"_f0")]]), size = 2) +
        labs(x = "time/Temp", y = "Freqs/Surface-Tension") +
        theme(legend.position="none") +
        ylim(ylim)

      sf_plot
    })

    output$viscosity_plot <- renderPlotly({
      spec_analysis_results <-
        spec_analysis_results() %>% dplyr::union(ephemeral_result())
      validate(need(spec_analysis_results, label = "spec_analysis_results"))
      spec_analysis_results %>%
        mutate(
          smoothed_temp = convert_to_temp(t, tevi_model()$tevi_data),
          viscosity = to_viscosity(d, sample_specs()$mass, sample_specs()$radius),
        ) %>%
        ggplot(aes(x = .data[[input$visc_xvar]], color = ephemeral)) +
        geom_point(aes(y = .data[[input$visc_yvar]]),  size = 2) +
        labs(x = "time/temp", y = "Damping/Viscosity") +
        theme(legend.position="none")
    })

    # results-table-output ---------
    output$spec_analsis_results_DT <- DT::renderDataTable({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        mutate(t = round(t, 2), wl = round(wl, 2)) %>%
        arrange(type, t) %>%
        select(-ephemeral)
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

