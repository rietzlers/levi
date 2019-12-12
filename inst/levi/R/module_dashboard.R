# module_dashboard.R #


dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"),
                  label = "select tevi-data (.dat-file)",
                  accept = c(".dat")),
    box(width = 12, collapsible = TRUE,
      title = "Set parameters",
      tibble::tribble(
        ~ id, ~ label,
        "frame_rate" , "Frame-Rate [Hz]",
        "sample_mass", "Sample-Mass [g]",
        "sphere_radius", "Sphere-Radius [mn]"
      ) %>%
        purrr::pmap( ~ numericInput(
          inputId = ns(.x),
          label = .y,
          value = NULL
        ))
    ),
    verbatimTextOutput(ns("raw_tevi_data_table"))

  ),
  # dashboard: display info --------------------
  box(width = 8,
    box(width =   12,
        plotOutput(ns("plot_center_xy"), height = 200)),
    box(width =  12,
        plotOutput(ns("plot_temp"), height = 200)),
    box(width =  12,
        plotOutput(ns("plot_heat_i"), height = 200))
  ))
}

dashboard <- function(input, output, session){

  raw_tevi_data <-
    reactive({import_tevi_data(session, input$file)})

  output$raw_tevi_data_table <-
    renderPrint({
      raw_tevi_data()
    })

  output$plot_center_xy <-
    renderPlot({
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red") +
        # geom_line(aes(y = htr_i_norm)) +
        labs(y = "Center-Coordinates [px]")
    })

  output$plot_temp <-
    renderPlot({
      test_data <-
        raw_tevi_data() %$%
        tibble(
          time_n = seq(min(time), max(time), length.out = 100),
          temp = to_temperature(tibble(time, pyro_temp), time_n)
        )

      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = pyro_temp)) +
        geom_point(data = test_data, aes(x = time_n, y = temp), color = "red")
    })

  output$plot_heat_i <-
    renderPlot({
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = htr_i))
    })


  # update ui ------------
  observeEvent(
    raw_tevi_data(),
    {
    # estimate sample/frame-rate from mean dt
    c(est_sample_freq) %<-%
      (raw_tevi_data() %>%
         summarize(est_sample_freq = round(1 / mean(diff(time), na.rm = TRUE))))

    updateNumericInput(session, "frame_rate", value = est_sample_freq)
   })

  list(
    raw_tevi_data,
    reactive(input$frame_rate)
  )
}
