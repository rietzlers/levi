# signal_plot.R #

signalAnalysisUI <- function(id, width = 12) {
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
          spectrumUI(ns("spectrum"))
          )
      )
  )
}


signalAnalysis <- function(input, output, session, raw_tevi_data, frame_rate){

  ns <- session$ns

  signal <- reactive(rlang::sym(req(input$signal_choice)))

  data_selection <-
    reactive({
      selected_data <-brushedPoints(raw_tevi_data(), input$brush)
      validate(need(nrow(selected_data) > 0,
                    "select data by brushing (left-click and pull) over signal-plot"))
      selected_data
    })

  # update UI -
  observeEvent(raw_tevi_data(), {
    col_names <- raw_tevi_data() %>% names()
    signal_names <- col_names[col_names != "time"]
    updateSelectInput(session, "signal_choice", choices = signal_names)
  })

  # display signal ------
  output$signal_plot <-
    renderPlot({ts_plot(raw_tevi_data(), input$signal_choice)})

  # signal in range ------------------
  output$signal_in_selected_range <-
    renderPlot({ts_plot(data_selection(), input$signal_choice)})

  # spectrum ---------
  callModule(spectrum_ctrl, "spectrum", data_selection, signal, frame_rate)

  # numerical summary --------------
  output$selected_signal_info <-
    renderUI({
      br <- get_brush_range(input$brush)
      h5(str_glue("Window center-point: {round(mean(br), 2)} s and window-width: {round(diff(br), 2)} s"))
    })
}


ts_plot <- function(ds, var){
  ds %>%
    ggplot(aes(x = time)) +
    geom_line(aes_string(y = var))
}
