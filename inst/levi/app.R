## app.R ##

source(file.path("R/load_dependencies.R"),
       local = TRUE,
       encoding = "UTF-8")$value

# header ------------------------------------------------------------------

header <-
  dashboardHeader(
    title = "levi",
    dropdownMenu(type = "notifications"),
    dropdownMenu(type = "messages"),
    dropdownMenu(type = "tasks")
  )



# sidebar ----------------------------------------------------------------
sidebar <-
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      "Display Signal",
      tabName = "display_signal",
      icon = icon("bar-chart-o")
    )

  ))

# body ------------------------
body <-
  dashboardBody(tabItems(
    # dashboard: select file --------------------
    tabItem(
      tabName = "dashboard",
      shinydashboard::box(
        width = 4,

        # upload file -
        shinydashboard::box(
          title = "select tevi-data (.dat-file)",
          width = 12,
          fileInput(
            "file",
            label = NULL,
            multiple = TRUE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              ".dat"
            )
          )
        ),

        shinydashboard::box(
          collapsible = TRUE,
          width = 12,
          title = "Set parameters",
          #boxed_numericInput("frame_rate", "Frame-Rate [Hz]"),
          !!!sample_spec_inputs
        ),

        # display data-table -
        shinydashboard::box(
          collapsible = TRUE,
          width = 12,
          title = "Imported data",
          verbatimTextOutput("raw_tevi_data_table")

        )
      ),
      # dashboard: display info --------------------
      shinydashboard::box(
        width = 8,
        shinydashboard::box(width =   12,
                            plotOutput("plot_center_xy", height = 200)),
        shinydashboard::box(width =  12,
                            plotOutput("plot_temp", height = 200)),
        shinydashboard::box(width =  12,
                            plotOutput("plot_heat_i", height = 200))
      )
    ),

    # display signal --------------------
    tabItem(
      tabName = "display_signal",
      box(
        width = 12,
        title = "Signal Plot",
        selectInput("signal_choice", label = "Select Signal", choices = NULL),
        plotOutput(
          "signal_plot",
          brush = brushOpts(
            id = "signal_plot_brush",
            fill = "#ccc",
            direction = "x"
          ),
          height = 250
        )
      ),
      box(
        title = "Signal in selected Range and corresponding spectrum",
        width = 12,
        box(width = 6,
            plotOutput("signal_in_selected_range",
                       height = 250)),
        box(width = 6,
            plotlyOutput("spectrum",
                         height = 250))
      ),
      box(width = 6,
          uiOutput("selected_signal_info")),
      box(width = 6,
          uiOutput("spectrum_info"))
    )

  ))


ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")

server <-
  function(input, output, session) {
    # reactive data ----
    raw_tevi_data <- reactive({import_tevi_data(session, input$file)})

    data_selection <- reactive({select_data(session, raw_tevi_data(), input$signal_plot_brush)})


    signal <- reactive(rlang::sym(req(input$signal_choice)))

    est_spec <- reactive(estimate_signal_spectrum(session, data_selection(), signal(), input$frame_rate))

    # dashboard  -----
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


    # display signal ------
    output$signal_plot <-
      renderPlot({
        raw_tevi_data() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = !!signal()))

      })

    # signal in range ------------------
    output$signal_in_selected_range <-
      renderPlot({
        data_selection() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = !!signal()))
      })

    # spectrum ---------
    output$spectrum <-
      renderPlotly({
        est_spec_plot <-
          est_spec() %>%
          ggplot(aes(x = freq, y = spec)) +
          geom_line() +
          labs(x = "Frequency [Hz]",
               y = "log(periodogram)")

        est_spec_plot %>%
          ggplotly()

      })

    # numerical summary --------------
    output$selected_signal_info <-
      renderUI({
        data <-
          data_selection() %>%
          select(time,!!signal())

        time_range <- data %$% range(time)
        time <- mean(time_range)
        time_span <- time_range[2] - time_range[1]

        {
          h5(
            str_glue(
              "Window center-point: {round(time, 2)} s and window-width: {round(time_span, 2)} s"
            )
          )
        }
      })

    output$spectrum_info <-
      renderUI({
        max_freq <-
          est_spec() %>%
          filter(near(spec, max(spec), tol = 0.5)) %>%
          summarize(max_freq = mean(freq))
        h5(str_glue("Maximum Freq.: {round(max_freq$max_freq, 2)} Hz"))
      })
  }

shinyApp(ui, server)
