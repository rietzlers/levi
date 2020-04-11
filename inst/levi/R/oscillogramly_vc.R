
oscillogramUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width =  1,
             selectInput(ns("selected_signal"), label = "Signal", choices = NULL),
             numericInput(ns("window_start"), label = "ws", value = 0 )),
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

  window <- reactiveVal()
  observeEvent(tevi_model(),
               window(
                 tevi_model()$tevi_data  %>%
                   pull(t) %>%
                   range(na.rm = TRUE)
               ))
  observeEvent(window_range(),{
    window(window_range())
  })
  observeEvent(input$window_start,{
    window(c(0,1) + input$window_start)
  })


  window_range <- reactive({
    xrange <- event_data("plotly_relayout")$xaxis.range
    validate(need(xrange, message = "select window in oscillogram"))
    xrange
    }) %>%
    debounce(1000)




  # observers -----------
  observeEvent(tevi_model(),{
    updateSelectInput(session, "selected_signal", label = "Signal",
                      choices = names(tevi_model()$tevi_data),
                      selected = "radius_y")
  }) #update signal-selection



  output$oscillogram <- renderPlotly({
    tevi_model()$tevi_data %>%
      plot_ly(x = ~t) %>%
      add_lines(
        y = ~get(input$selected_signal)
        ) %>%
      layout(
        yaxis = list(title = input$selected_signal),
        xaxis = list(title = "time [s]",
                     range = window())
      ) %>%
      rangeslider(thickness = .4) %>%
      event_register("plotly_relayout")
  })

  # return-values -----------

  list(
    data_selection,
    signal_name,
    window
  )
}
