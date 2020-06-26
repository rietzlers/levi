
oscillogramUI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width =  1, selectInput(ns("selected_signal"), label = "Signal", choices = NULL)),
          column(width = 11, plotlyOutput(ns("oscillogram"), height = "300px"))
        )
    ),
    bsModal(ns("osci_ctrls"), title = "Additional Oscillogram Controls", trigger = ns("show_ctrls"),
            numericInput(ns("window_step_size"), label = "Set step-size [%]", value = 50, min = 0, max = 200, step = 10),
            #selectInput(ns("bp_choices"), label = "Show Signals", choices = c("Raw" = "raw", "BP-Filterd" = "bp_filtered", "Both" = "both")),
            size = "large")
  )
}

oscillogram_ctrl <- function(input, output, session, tevi_model){

  # local-data ----
  signal_name  = reactive({
    validate(need(input$selected_signal, label = "signal to analyse"))
    input$selected_signal
  })

  observeEvent(tevi_model(), {
    updateSelectInput(
      session,
      "selected_signal",
      label = "Signal",
      choices = names(tevi_model()$analysis_data),
      selected = "radius_y"
    )
  })

  window_range <- reactive({
    event_data("plotly_relayout", source = "osci-plot")$xaxis.range
  }) %>% debounce(1000)

  output$oscillogram <- renderPlotly({
    tevi_model()$analysis_data %>%
      plot_ly(x = ~t, source = "osci-plot") %>%
      add_lines(
        y = ~get(signal_name()),
        hoverinfo = "none"
        ) %>%
      layout(
        yaxis = list(title = input$selected_signal),
        xaxis = list(
          title = "time [s]",
          range = range(tevi_model()$analysis_data[["t"]], nar.rm = TRUE),
          rangeslider = list(
            thickness = 0.3,
            yaxis = list(rangemode = "auto")
          )
        )
      ) %>%
      config(displaylogo = FALSE) %>%
      event_register("plotly_relayout")
  })

  # return-values -----------

  list(
    signal_name,
    reactive({
      validate(need(window_range(), message = "set time-window-range in oscillogram"))
      window_range()
      })
  )
}
