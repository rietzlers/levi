
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
        plotlyOutput(ns("temp_plot"), height = 300),
        plotOutput(
          ns("heat_pulses_plot"), height = 100,
          brush = brushOpts(id = ns("heat_pulses_plot_brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE)
        ),
        fluidRow(
          column(width = 2, actionButton(ns("save_hp"), label = "save hp Nr.:")),
          column(width = 3, numericInput(ns("hp_nr"), label = "", value = 1)),
          column(width = 5, textOutput(ns("hp_range")))
        )
    )
  )
}

load_tevi_data_ctrl <- function(input, output, session) {
  ns <- session$ns

  # tevi-data --------
  tevi_data_RV <- reactiveVal()

  observeEvent(input$file, {
    # update raw-tevi-data
    tryCatch({
      input$file$datapath %>% import_tevi_data() %>% tevi_data_RV()
      showNotification(
        "Tevi-data uploaded: Make sure to that the right sample-specs are selected!",
        duration = 10, type = "warning"
      )

      # estimate sample/frame-rate from mean dt
      c(est_sample_freq) %<-%
        (tevi_data() %>% summarize(est_sample_freq = round(1 / mean(diff(t), na.rm = TRUE))))

      updateNumericInput(
        session, "frame_rate",
        value = est_sample_freq
      )

      showNotification(
        HTML(str_glue("Estimated frame-rate for data from file '{input$file$name}' is: <b>{est_sample_freq} Hz </b>.")),
        duration = 10, type = "message")
    },
    error = function(e){
      showNotification(e$message, duration = NULL)
    }
    )
  })

  tevi_data <-
    reactive({
      validate(need(tevi_data_RV(), message = "No Tevi-data available! Make sure you have uploaded a .csv-Tevi-file."))
      tevi_data_RV()
    })

  observeEvent(
    exp_time_range(),
    tevi_data_RV(tevi_data_RV() %>% add_temperature(time_range = exp_time_range())),
    ignoreInit = TRUE
  )

  # frame-rate ------
  frame_rate = reactive({
    validate(need(input$frame_rate, label = "frame_rate"))
    input$frame_rate
  })

  # analysis-data/exp-time-range--------------
  exp_time_range <- reactiveVal(NULL)
  observeEvent(
    event_data("plotly_brushed", source = ns("temp_plot")),
    exp_time_range(event_data("plotly_brushed", source = ns("temp_plot"))$x),
    ignoreInit = TRUE
  )

  analysis_data <- reactive({
    validate(need(exp_time_range(), message = "No analysis-data as there is no exp_time_range defined! Define one in the load-tevi-data tab (Dashboard)"))
    tevi_data() %>%
      filter(t %>% between(exp_time_range()[1], exp_time_range()[2])) %>%
      mutate(t = t - min(t, na.rm = TRUE))
  })

  # Heat-Puls ---------
  hps <- reactiveValues()
  HPs = reactive({
    validate(need(hps, label = "heat-puls-data"))
    reactiveValuesToList(hps)
  })

  observeEvent(input$save_hp,{
    hp_nr <- input$hp_nr
    hps[[str_glue("hp{hp_nr}")]] <- get_brush_range(input$heat_pulses_plot_brush, str_glue("brush heat-puls-plot to set hp {hp_nr}"))
  }) # update heat-pulses

  # bookmark-callbacks ---------------
  onBookmark(function(state){
    state$values$exp_time_range <- exp_time_range()
  })
  onRestored(function(state){
    exp_time_range(state$values$exp_time_range)
  })
  # plot_center_xy  -------------
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
    if(is.null(need(exp_time_range(), "exp_time_range"))){
      center_xy_plot <-
        center_xy_plot +
        geom_vline(xintercept = exp_time_range(), linetype = "dashed", color = "blue")
    }
    center_xy_plot
  })

  # temp_plot ------
  output$temp_plot <- renderPlotly({

    tevi_data() %>%
      plot_ly(x  = ~t, source = ns("temp_plot")) %>%
      add_lines(y = ~pyro_temp, name = "Measured", hovertemplate = "%{y:.0f} °C") %>%
      add_lines(
        name =
          "Smoothed Temp.
      (used for time - Temp conversion)",
        y = ~smoothed_temp,
        color = I("red"),
        hovertemplate = "%{y:.0f} °C"
      ) %>%
      layout(
        legend = list(x = 0.8, y = 0.8),
        dragmode = "select",
        selectdirection = "h",
        xaxis = list(title = "time [s]"),
        yaxis = list(title = "Pyro-Temp. [°C]")
      ) %>%
      event_register("plotly_brushed")
  })
  # heat-puls-plot --------
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
    validate(need(tevi_data_RV(),
                  message = "No Tevi-data available! Make sure you have uploaded a .csv-Tevi-file."))
    validate(need(analysis_data(),
                  message = "No analysis-time-range specified. Brush temp-plot in 'dashboard'-tab 'upload tevi-data' to set experimental time-range"))
    list(
      tevi_data = tevi_data(),
      analysis_data = analysis_data(),
      tevi_data_name = input$file$name,
      exp_time_range = exp_time_range(),
      HPs = HPs(),
      frame_rate = frame_rate()
    )}
  )
}
