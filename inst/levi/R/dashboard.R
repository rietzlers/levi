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
          column(2, selectInput(ns("hp"), label = NULL, choices = c("one" = "hp1", "two" = "hp2", "three" = "hp3"))),
          column(2, actionButton(ns("save"), label = "save", icon = icon("archive"))),
          column(8, verbatimTextOutput(ns("hp_range")) )
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
  c(frame_rate) %<-% callModule(parameters, "params", raw_tevi_data)

  output$raw_tevi_data_table <- renderPrint({raw_tevi_data()})

  output$plot_center_xy <-
    renderPlot({
      raw_tevi_data() %>%
        ggplot(aes(x = time)) +
        geom_line(aes(y = center_x)) +
        geom_line(aes(y = center_y), color = "red")
    })

  ss_times <-  callModule(signal, "temp", raw_tevi_data)

  heating_brush <- callModule(signal, "heating", raw_tevi_data)

  output$hp_range <-
    renderPrint({
      str_glue("{hp_times[[input$hp]]}")
      })

  hp_times <- reactiveValues()

  observeEvent(input$save,
               hp_times[[input$hp]] <-  heating_brush()
               )
  list(
    raw_tevi_data,
    frame_rate
  )
}



