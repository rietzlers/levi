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
        signalUI(ns("temp")),
        signalUI(ns("heating")),
        fluidRow(
          column(2, selectInput(ns("heat_pulse_choice"), label = NULL, choices = c("one" = "hp1", "two" = "hp2", "three" = "hp3"))),
          column(2, actionButton(ns("save_timing_info"), label = "save", icon = icon("archive"))),
          column(8, verbatimTextOutput(ns("heat_pulse_range")) )
          )
        )
    )
}


#' dashboard
#'
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with: reactives: raw_tevi_data (tibble), experiment-meta-info (list)

dashboard <- function(input, output, session, data, frame_rate){

  raw_tevi_data <- reactive({import_tevi_data(session, input$file)})

  model <- reactiveValues()

  observeEvent(input$save_timing_info,
               {
                 model[[input$heat_pulse_choice]] <- isolate(heating_brush)
               })

  c(frame_rate) %<-% callModule(parameters, "params", raw_tevi_data)

  output$raw_tevi_data_table <- renderPrint({raw_tevi_data()})

  output$plot_center_xy <-
    renderPlot({
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red")
    })

  model$ss_times <-  callModule(signal, "temp", raw_tevi_data)

  heating_brush <- callModule(signal, "heating", raw_tevi_data)

  output$heat_pulse_range <-
    renderPrint({
      model[[input$heat_pulse_choice]]()
    })


  list(
    raw_tevi_data,
    frame_rate
  )
}



