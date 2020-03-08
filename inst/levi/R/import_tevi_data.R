
importTeviDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        model_params_view(ns("params"))),
    box(width = 12,
        plotOutput(ns("plot_center_xy"), height = 200),
        div(plotOutput(ns("temp_plot"), height = 300,
                       brush = brushOpts(id = ns("temp_plot_brush"), delayType = "debounce", delay = 1000, fill = "#ccc", direction = "x", resetOnNew = FALSE)),
            textOutput(ns("exp_time_range_info"))),
        div(plotOutput(ns("heat_pulses_plot"), height = 100,
                      brush = brushOpts(id = ns("heat_pulses_plot_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)),
            fluidRow(
              column(width = 2, actionButton(ns("save_hp"), label = "save hp Nr.:")),
              column(width = 3, numericInput(ns("hp_nr"), label = "", value = 1)),
              column(width = 5, textOutput(ns("hp_range")))
            )
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
  tevi_data_name <- reactive({
    validate(need(input$file, "tevi_data"))
    input$file$name
  })
  tevi_data <- reactive({
    imported_tevi_data() %>%
      add_temperature(time_range = exp_time_range())
  })
  hps <- reactiveValues()
  exp_time_range <- reactive({
     get_brush_range(input$temp_plot_brush,
                     "brush temp-plot to set experimental time-range",
                     default_values = range(imported_tevi_data()$t))
  })
  observeEvent(input$save_hp,{
    hp_nr <- input$hp_nr
    hps[[str_glue("hp{hp_nr}")]] <- get_brush_range(input$heat_pulses_plot_brush, str_glue("brush heat-puls-plot to set hp {hp_nr}"))
  })

  # output-ctrls -------------
  c(frame_rate, mass, radius) %<-% callModule(model_params_ctrl, "params", tevi_data)
  output$plot_center_xy <- renderPlot({
    center_xy_plot <-
        tevi_data() %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red") +
        labs(y = "center-coordinates")
    hps <- reactiveValuesToList(hps) %>% flatten_dbl()
    if(is.null(need(hps, "hps"))){
      center_xy_plot <-
        center_xy_plot +
        geom_vline(xintercept = hps, linetype = "dashed", color = "red")
    }
    center_xy_plot
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
  output$hp_range <- renderText({
    hps[[str_glue("hp{input$hp_nr}")]]
  })
  # return-values ----------
  list(
    tevi_data,
    tevi_data_name,
    exp_time_range,
    frame_rate,
    mass,
    radius
    )
}



