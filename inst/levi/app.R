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
    tabItem(tabName = "display_signal", signalPlotUI("signal1"))

  ))


ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")

server <-
  function(input, output, session) {
    # reactive data ----
    raw_tevi_data <- reactive({import_tevi_data(session, input$file)})

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

    callModule(signalPlot, "signal1", reactive(raw_tevi_data()), reactive(input$frame_rate))
  }

shinyApp(ui, server)
