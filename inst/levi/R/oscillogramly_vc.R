
oscillogramUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width =  1,
             selectInput(ns("selected_signal"), label = "Signal", choices = NULL),
             numericInput(ns("window_start"), label = "Window-Start", value = 0, min = 0),
             numericInput(ns("window_length"), label = "Window-Length", value = 1, min = 0.1, step = 0.1)),
      column(width = 11, plotlyOutput(ns("oscillogram")))
    )
  )
}

oscillogram_ctrl <- function(input, output, session, tevi_model){

  # local-data ----
  signal_name  = reactive({input$selected_signal})
  data_selection <- reactive({
    validate(need(window(), "window"))
    tevi_model()$tevi_data %>%
      filter(t %>% between(window()[1], window()[2]))
  }) %>%
    debounce(500)
  window_range <- reactive({
    xrange <- event_data("plotly_relayout")$xaxis.range
    validate(need(xrange, message = "select window in oscillogram"))
    xrange
    }) %>%
    debounce(1000)
  window <- reactiveVal()

  # observers -----------
  observeEvent(tevi_model(),{
    updateSelectInput(session, "selected_signal", label = "Signal",
                      choices = names(tevi_model()$tevi_data),
                      selected = "radius_y")
  }) #update signal-selection
  observeEvent(tevi_model(), {
               window(range(tevi_model()$tevi_data[["t"]], na.rm = TRUE))
    }) # init window
  observeEvent(window_range(),{
    window(window_range())
  }) # update window with range-slider
  observeEvent({input$window_start; input$window_length},{
    window(c(0, input$window_length) + input$window_start)
  }) # update window via input
  # bookmark-observers ----------
  onBookmark(function(state){
    state$values$window <- window()
  })
  onRestored(function(state){
    window(state$values$window)
  })

  output$oscillogram <- renderPlotly({
    tevi_model()$tevi_data %>%
      plot_ly(x = ~t) %>%
      add_lines(
        y = ~get(input$selected_signal),
        hoverinfo = "none"
        ) %>%
      layout(
        yaxis = list(title = input$selected_signal),
        xaxis = list(title = "time [s]",
                     range = window())
      ) %>%
      rangeslider(
        thickness = .4,
        rangemode = "auto"
        ) %>%
      event_register("plotly_relayout")
  })

  # return-values -----------

  list(
    data_selection,
    signal_name,
    window
  )
}
