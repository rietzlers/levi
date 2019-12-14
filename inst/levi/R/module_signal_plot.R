# module_signal_plot.R #

signalPlotUI <- function(id, width = 12) {
  ns <- NS(id)

  #browser()
  tagList(
    box(width = width,
      selectInput(ns("signal_choice"), label = "Choose Signal to analyse", choices = NULL),
      plotOutput(ns("signal_plot"),
        brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x"),
        height = 250
      ),
      box(width = 6,
          plotOutput(ns("signal_in_selected_range"),  height = 250),
          uiOutput(ns("selected_signal_info"))
          ),
      box(width = 6,
          plotlyOutput(ns("spectrum"), height = 250),
          uiOutput(ns("spectrum_info")),
          verbatimTextOutput(ns("spectrum_click_info"))
          )
      )
  )
}


signalPlot <- function(input, output, session, raw_tevi_data, frame_rate){

  ns <- session$ns

  signal <- reactive(rlang::sym(req(input$signal_choice)))
  est_spec <- reactive({
    estimate_signal_spectrum(data_selection(), signal(), frame_rate())
    })

  data_selection <-
    reactive({
      selected_data <-
        brushedPoints(raw_tevi_data(), input$brush)
      validate(need(nrow(selected_data) > 0,
                    "select data by brushing (left-click and pull) over signal-plot"))
      selected_data
    })

  # update UI -
  observeEvent(raw_tevi_data(), {
    # available signals
    col_names <- raw_tevi_data() %>% names()
    signal_names <- col_names[col_names != "time"]
    updateSelectInput(session, "signal_choice", choices = signal_names)
  })

  # display signal ------
  output$signal_plot <-
    renderPlot({
      ts_plot(raw_tevi_data(), input$signal_choice)
    })

  # signal in range ------------------
  output$signal_in_selected_range <-
    renderPlot({
      ts_plot(data_selection(), input$signal_choice)
    })

  # spectrum ---------
  output$spectrum <-
    renderPlotly({
      est_spec_plot <-
        est_spec() %>%
        ggplot(aes(x = freq, y = spec)) +
        geom_line() +
        labs(x = "Frequency [Hz]",
             y = "log(periodogram)")

      est_spec_plot %>%
        ggplotly(source = ns("spectrum_plotly")) %>%
        event_register("plotly_click")

    })

  # numerical summary --------------
  output$selected_signal_info <-
    renderUI({
      data <-
        data_selection() %>%
        select(time,!!signal())

      time_range <- data %$% range(time)
      time <- mean(time_range)
      time_span <- time_range[2] - time_range[1]

      {
        h5(
          str_glue(
            "Window center-point: {round(time, 2)} s and window-width: {round(time_span, 2)} s"
          )
        )
      }
    })

  output$spectrum_info <-
    renderUI({
      max_freq <-
        est_spec() %>%
        filter(near(spec, max(spec), tol = 0.5)) %>%
        summarize(max_freq = mean(freq))
      h5(str_glue("Maximum Freq.: {round(max_freq$max_freq, 2)} Hz"))
    })

  output$spectrum_click_info <- renderPrint({
    #hover_info <- event_data("plotly_hover", source = "spectrum_plotly")
    click_info <- event_data("plotly_click", source = ns("spectrum_plotly"))
    if (is.null(click_info)) "Click events appear here (double-click to clear)" else click_info
  })
}


ts_plot <- function(ds, var){
  ds %>%
    ggplot(aes(x = time)) +
    geom_line(aes_string(y = var))
}
