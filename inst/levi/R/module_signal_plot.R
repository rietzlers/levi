# module_signal_plot.R #

signalPlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    box(width = 12,
      selectInput(ns("signal_choice"), label = "Choose Signal to analyse", choices = NULL),
      plotOutput(ns("signal_plot"),
        brush = brushOpts(id = ns("signal_plot_brush"), fill = "#ccc", direction = "x"),
        height = 250
      ),
      box(width = 6,
          plotOutput(ns("signal_in_selected_range"),  height = 250),
          uiOutput(ns("selected_signal_info"))
          ),
      box(width = 6,
          plotlyOutput(ns("spectrum"), height = 250),
          uiOutput(ns("spectrum_info"))
          )
      )
  )
}


signalPlot <- function(input, output, session, raw_tevi_data, frame_rate){

  signal <- reactive(rlang::sym(req(input$signal_choice)))

  data_selection <- reactive({select_data(session, raw_tevi_data(), input$signal_plot_brush)})

  est_spec <- reactive(estimate_signal_spectrum(data_selection(), signal(), frame_rate()))

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
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = !!signal()))

    })

  # signal in range ------------------
  output$signal_in_selected_range <-
    renderPlot({
      data_selection() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = !!signal()))
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
        ggplotly()

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
}
