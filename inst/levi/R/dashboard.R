
dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 4,
        fileInput(ns("file"), label = "select tevi-data (.dat-file)", accept = c(".dat")),
        parametersUI(ns("params"))),
    box(width = 8,
        plotOutput(ns("plot_center_xy"), height = 200),
        signalUI(ns("temp")),
        signalUI(ns("heating")),
        fluidRow(
          column(2, selectInput(ns("hp"), label = NULL, choices = c("one" = "hp1", "two" = "hp2", "three" = "hp3"))),
          column(8, verbatimTextOutput(ns("exp_timing")))))
    )
}

dashboard <- function(input, output, session){

  tevi_data <- reactive({import_tevi_data(session, input$file)})

  gen_exp_timing <- function(){
    exp_timing <-
      reactiveValues(
        lc = c(NA_real_, NA_real_),
        hp1 = c(NA_real_, NA_real_),
        hp2 = c(NA_real_, NA_real_),
        hp3 = c(NA_real_, NA_real_))

    observe({
      exp_timing$lc <-  liquid_cooling()
      exp_timing[[isolate(input$hp)]] <- hp_range()
      })
    # reset exp_timing with new data
    observeEvent(input$file, {
      for(name in names(exp_timing)){exp_timing[[name]] <- c(NA_real_, NA_real_)}})

    reactive(as_tibble(reactiveValuesToList(exp_timing)) %>% select(lc, hp1, hp2, hp3))
  }

  exp_timing <- gen_exp_timing()


  output$plot_center_xy <-
    renderPlot({center_coords(tevi_data(), exp_timing())})

  liquid_cooling <-
    callModule(signal, "temp", tevi_data, "pyro_temp")

  hp_range <-
    callModule(signal, "heating", tevi_data, "htr_i")

  c(frame_rate) %<-%
    callModule(parameters, "params", tevi_data)

  output$exp_timing <-
    renderPrint({exp_timing()})

  list(
    tevi_data,
    frame_rate
  )
}

center_coords <-
  function(data, exp_timing){
    data %>%
    ggplot(aes(x = time)) +
      geom_line(aes(y = center_x)) +
      geom_line(aes(y = center_y), color = "red") +
      geom_vline(data = exp_timing, aes(xintercept = lc), linetype = "dashed") +
      geom_vline(data = exp_timing  %>% pivot_longer(cols = starts_with("hp"), values_to = "hp", values_drop_na = TRUE),
                 aes(xintercept = hp), linetype = "dashed", color = "red")
  }

