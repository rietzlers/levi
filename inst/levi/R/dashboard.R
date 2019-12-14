# module dashboard.R #


#' dashboardUI
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return tagList with inputs for file-upload, experiment meta-info
#' and outputs for visual display of importand variables
dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
    parametersUI(ns("params")),
    verbatimTextOutput(ns("raw_tevi_data_table"))

  ),
  # dashboard: display info --------------------
  box(width = 8,
      plotOutput(ns("plot_center_xy"), height = 200),
      plotOutput(ns("plot_temp"), height = 200, brush = brushOpts(id = ns("plot_temp_brush"), fill = "#ccc", direction = "x")),
      verbatimTextOutput(ns("time_range")),
      plotOutput(ns("plot_heat_i"), height = 200,brush = brushOpts(id = ns("plot_heat_brush"), fill = "#ccc", direction = "x")),
      fluidRow(
        column(2, selectInput(ns("heat_pulse_choice"), label = NULL, choices = c("one" = "one", "two" = "two", "three" = "three"))),
        column(8, verbatimTextOutput(ns("heat_pulse_range"))),
        column(2, actionButton(ns("save_timing_info"), label = "save", icon = icon("archive")))
      )
  ))
}


#' dashboard
#'
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with: reactives: raw_tevi_data (tibble), experiment-meta-info (list)

dashboard <- function(input, output, session, model){

  raw_tevi_data <- reactive({import_tevi_data(session, input$file)})

  timing_info <- reactiveValues()

  observeEvent(input$save_timing_info,
               {
                 timing_info$ss_times <- get_brush_range(input$plot_temp_brush)
                 timing_info[[input$heat_pulse_choice]] <-
                   get_brush_range(input$plot_heat_brush)
               })

  c(model$frame_rate) %<-% callModule(parameters, "params", raw_tevi_data)

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

  output$time_range <-
    renderPrint({timing_info$ss_times})

  output$plot_heat_i <-
    renderPlot({
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = htr_i))
    })

  output$heat_pulse_range <-
    renderPrint({timing_info[[input$heat_pulse_choice]]})



  list(
    raw_tevi_data,
    timing_info
  )
}



