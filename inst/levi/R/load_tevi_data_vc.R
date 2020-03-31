
load_tevi_data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 8, fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat"))),
          column(width = 4, numericInput(ns("frame_rate"), label = "Frame Rate [Hz]", value = NULL))
        )
    ),
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

load_tevi_data_ctrl <- function(input, output, session) {
  # local data --------
  tevi_data_RV <- reactiveVal()
  tevi_data <- reactive({
    validate(need(tevi_data_RV(), message = "No Tevi-data available! Make sure you have uploaded a .csv-Tevi-file."))
    tevi_data_RV()
  })
  tevi_data_name <- reactive({
    validate(need(input$file, message = "select file in file-upload-dialog"))
    input$file$name
  })
  exp_time_range <- reactive({
    get_brush_range(input$temp_plot_brush,
                    "brush temp-plot to set experimental time-range",
                    default_values = range(tevi_data()$t))
  })
  frame_rate = reactive({
    validate(need(input$frame_rate, label = "frame_rate"))
    input$frame_rate
  })
  hps <- reactiveValues()
  HPs = reactive({
    validate(need(hps, label = "heat-puls-data"))
    reactiveValuesToList(hps)
  })

  # observers ------
  observeEvent(input$file, {
    tevi_data <- import_tevi_data(input$file$datapath)
    # estimate sample/frame-rate from mean dt
    c(est_sample_freq) %<-%
      (tevi_data %>% summarize(est_sample_freq = round(1 / mean(diff(t), na.rm = TRUE))))
    updateNumericInput(session, "frame_rate", value = est_sample_freq)
    showNotification(
      HTML(str_glue("Estimated frame-rate for data from file '{tevi_data_name()}' is: <b>{est_sample_freq} Hz </b>.")),
      duration = 5, type = "message")
    showNotification(
      "Tevi-data uploaded: Make sure to that the right sample-specs are selected!",
      duration = 5, type = "warning")
    tevi_data_RV(tevi_data)
    session$resetBrush(session$ns("temp_plot_brush"))
  }) # load-tevi-data on input-file-change
  observeEvent(exp_time_range(),{
      tevi_data_RV(tevi_data() %>% add_temperature(time_range = exp_time_range()))
    }, ignoreInit = TRUE)# add temp-var to tevi_data
  observeEvent(input$save_hp,{
    hp_nr <- input$hp_nr
    hps[[str_glue("hp{hp_nr}")]] <- get_brush_range(input$heat_pulses_plot_brush, str_glue("brush heat-puls-plot to set hp {hp_nr}"))
  }) # update heat-pulses


  # output-ctrls -------------
  output$plot_center_xy <- renderPlot({
    center_xy_plot <-
        tevi_data() %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red") +
        labs(y = "center-coordinates")
    if(is.null(need(hps, "hps"))){
      center_xy_plot <-
        center_xy_plot +
        geom_vline(xintercept = HPs() %>% flatten_dbl(), linetype = "dashed", color = "red")
    }
    if(is.null(need(exp_time_range, "exp_time_range"))){
      center_xy_plot <-
        center_xy_plot +
        geom_vline(xintercept = exp_time_range(), linetype = "dashed", color = "blue")
    }
    center_xy_plot
    })
  output$temp_plot <- renderPlot({
    tevi_data() %>%
        ggplot(aes(x = t)) +
        geom_line(aes(y = pyro_temp)) +
        geom_line(aes(y = smoothed_temp), color = "red")
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
  reactive({
    list(
      tevi_data = tevi_data(),
      tevi_data_name = tevi_data_name(),
      exp_time_range = exp_time_range(),
      HPs = HPs(),
      frame_rate = frame_rate()
    )}
  )
}



