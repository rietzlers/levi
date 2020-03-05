
importTeviDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        model_params_view(ns("params"))),
    box(width = 8,
        plotOutput(ns("plot_center_xy"), height = 200),
        box(width = 12,{
          div(
            plotOutput(ns("temp_plot"),
                       brush = brushOpts(id = ns("temp_plot_brush"), fill = "#ccc", direction = "x",
                                         delay = 500, delayType = "debounce", resetOnNew = FALSE)),
            fluidRow(
             column(6, textOutput(ns("exp_time_range_info")))
            )
          )}),
        signalUI(ns("heating")))
  )
}

importTeviData <- function(input, output, session) {

  # local data --------
  imported_tevi_data <- reactive({
    validate(need(input$file, "tevi_data"))
    import_tevi_data(session, input$file)
  })

  tevi_data <- reactive({
    imported_tevi_data() %>%
      add_temperature(time_range = exp_time_range())
  })


  exp_time_range <- reactive({
     get_brush_range(input$temp_plot_brush,
                     "brush temp-plot to set experimental time-range",
                     default_values = range(imported_tevi_data()$t))
  })

  # output-ctrls -------------
  output$plot_center_xy <- renderPlot({
      tevi_data() %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red") +
        labs(y = "center-coordinates")
    })

  output$temp_plot <- renderPlot({
    plot <- function(ds){
      ds %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = pyro_temp)) +
        geom_line(aes(y = smoothed_temp), color = "red")
    }
    plot(tevi_data())
  })
  output$exp_time_range_info <- renderText(exp_time_range())

  c(...skip, hp_brush) %<-%
    callModule(signal_ctrl, "heating", tevi_data, "htr_i")
c(frame_rate) %<-%
    callModule(model_params_ctrl, "params", tevi_data)

  # return-values ----------
  list(tevi_data,
       frame_rate)
}



