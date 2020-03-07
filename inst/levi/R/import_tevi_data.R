
importTeviDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        model_params_view(ns("params"))),
    box(width = 12,
        plotOutput(ns("plot_center_xy"), height = 200),
        div(plotOutput(ns("temp_plot"), height = 300,
                       brush = brushOpts(id = ns("temp_plot_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
            textOutput(ns("exp_time_range_info"))),
        div(plotOutput(ns("heat_pulses_plot"), height = 100,
                      brush = brushOpts(id = ns("heat_pulses_plot_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
            actionButton(ns("set_hp"), label = "set heat-puls"),
            actionButton(ns("clear_hps"), label = "reset heat-pulses"),
            textOutput(ns("heat_pulses_info"))
            )
        )
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
  c(frame_rate, mass, radius) %<-% callModule(model_params_ctrl, "params", tevi_data)
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
  output$exp_time_range_info <- renderText({
    ss <- exp_time_range() %>% round(2)
    str_glue("Brush pyro-temp-plot to set the times for temperature-smoothing. Selected Range: [{ss[1]}, {ss[2]}]")
    })
  output$heat_pulses_plot <- renderPlot({
    tevi_data() %>%
      ggplot(aes(x = t, y = htr_i)) +
      geom_line()
  })
  output$heat_pulses_info <- renderText({
    "brush to set heat-pulses"
  })
  # return-values ----------
  list(
    tevi_data,
    exp_time_range,
    frame_rate,
    mass,
    radius
    )
}



