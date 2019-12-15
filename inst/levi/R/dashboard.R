
dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        parametersUI(ns("params")),
        verbatimTextOutput(ns("tevi_data_table"))),
    box(width = 8,
        plotOutput(ns("plot_center_xy"), height = 200),
        signalUI(ns("temp")),
        signalUI(ns("heating")),
        fluidRow(
          column(2, selectInput(ns("hp"), label = NULL, choices = c("one" = "hp1", "two" = "hp2", "three" = "hp3"))),
          column(2, actionButton(ns("save"), label = "save", icon = icon("archive"))),
          column(8, verbatimTextOutput(ns("exp_timing")) )))
    )
}

dashboard <- function(input, output, session){

  tevi_data <- reactive({import_tevi_data(session, input$file)})
  exp_timing <- reactiveValues()

  observeEvent({input$save; liquid_cooling()},{
      exp_timing[["liquid_cooling"]] <-  liquid_cooling()
      exp_timing[[input$hp]] <- hp_range()})

  output$tevi_data_table <- renderPrint({tevi_data()})

  output$plot_center_xy <-
    renderPlot({center_coords(tevi_data(), as_tibble(reactiveValuesToList(exp_timing)))})

  liquid_cooling <-  callModule(signal, "temp", tevi_data)
  hp_range <- callModule(signal, "heating", tevi_data)
  c(frame_rate) %<-% callModule(parameters, "params", tevi_data)

  output$exp_timing <-
    renderPrint({as_tibble(reactiveValuesToList(exp_timing))})

  list(
    tevi_data,
    frame_rate
  )
}

center_coords <-
  function(data, exp_timing){
    p <-
    data %>%
    ggplot(aes(x = time)) +
      geom_line(aes(y = center_x)) +
      geom_line(aes(y = center_y), color = "red")


    if(c("liquid_cooling") %in% names(exp_timing)){
      p <- p + geom_vline(data = exp_timing, aes(xintercept = liquid_cooling), linetype = "dashed")
    }
    p
  }

