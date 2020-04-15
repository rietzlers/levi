
oscillogramUI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
    fluidRow(
      column(width =  1,
             selectInput(ns("selected_signal"), label = "Signal", choices = NULL),
             numericInput(ns("window_start"), label = "Window-Start", value = 0, min = 0),
             numericInput(ns("window_length"), label = "Window-Length", value = 1, min = 0.1, step = 0.1)),
      column(width = 11, plotlyOutput(ns("oscillogram"), height = "250px"))
    )
    )
  )
}

oscillogram_ctrl <- function(input, output, session, tevi_model){

  # local-data ----
  signal_name  = reactive({
    validate(need(input$selected_signal, label = "signal to analyse"))
    input$selected_signal
    })

  xaxis_range <- reactive({
    event_data("plotly_relayout", source = "osci-plot")$xaxis.range
    }) %>% debounce(1000)

  t_axis_range <- reactiveVal({})

  # observe tevi-model-changes -----------
  observeEvent(tevi_model(),{
    updateSelectInput(session, "selected_signal", label = "Signal",
                      choices = names(tevi_model()$tevi_data),
                      selected = "radius_y")
    t_axis_range(range(tevi_model()$tevi_data[["t"]], nar.rm = TRUE))
  })

  # observe window-parameter-inputs -----
  observeEvent({input$window_start; input$window_length},{
    t_axis_range(c(0, input$window_length) + input$window_start)
  }) # update t_axis_range via input

  observeEvent(xaxis_range(),{
    t_axis_range(xaxis_range())
    updateNumericInput(session, "window_start", value = xaxis_range()[1])
    updateNumericInput(session, "window_length", value = diff(xaxis_range()))
  })

  # bookmark-observers ----------
  onBookmark(function(state){
    state$values$t_axis_range <- t_axis_range()
  })
  onRestored(function(state){
    t_axis_range(state$values$t_axis_range)
  })

  output$oscillogram <- renderPlotly({
    tevi_model()$tevi_data %>%
      plot_ly(x = ~t, source = "osci-plot") %>%
      add_lines(
        y = ~get(signal_name()),
        hoverinfo = "none"
        ) %>%
      layout(
        yaxis = list(title = input$selected_signal),
        xaxis = list(
          title = "time [s]",
          range = input$window_start + c(0, input$window_length),
          rangeslider = list(
            thickness = 0.15,
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
      validate(need(t_axis_range(), message = "set time-window-range in oscillogram"))
      t_axis_range()
      })
  )
}
