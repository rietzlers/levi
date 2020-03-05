
importTeviDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        model_params_view(ns("params"))),
    box(width = 8,
        plotOutput(ns("plot_center_xy"), height = 200),
        plotOutput(ns("temp_plot"), brush = brushOpts(id = ns("temp_plot_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
        signalUI(ns("heating")))
  )
}

importTeviData <- function(input, output, session) {
  # local data --------
  tevi_data <- reactive({
    import_tevi_data(session, input$file)
  })

  output$plot_center_xy <-
    renderPlot({
      tevi_data() %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red") +
        labs(y = "center-coordinates")
    })


  output$temp_plot <- renderPlot({
    tevi_data() %>%
      ggplot(aes(x = t)) +
      geom_line(aes(y = pyro_temp)) +
      geom_line(aes(y = smoothed_temp))
  })

  c(...skip, hp_brush) %<-%
    callModule(signal_ctrl, "heating", tevi_data, "htr_i")

  c(frame_rate) %<-%
    callModule(model_params_ctrl, "params", tevi_data)

  # return-values ----------
  list(tevi_data,
       frame_rate)
}



