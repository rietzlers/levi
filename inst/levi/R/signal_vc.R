# signal_vc.R #

# view ------------
signalUI <- function(id, height = 200){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4, selectInput(ns("selected_signal"), label = NULL, choices = NULL))
    ),
    plotOutput(
      ns("signal"), height = height,
      brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = TRUE))
    )
}

# controller ----------
signal_ctrl <- function(input, output, session, data, variable = NULL){

  observeEvent(data(),
      updateSelectInput(session, "selected_signal", choices = names(data()), selected = variable))

  output$signal <-
    renderPlot({gen_signal_plot(data(), input$selected_signal)})

   # return-values ---------
  list(
    signal_name  = reactive({input$selected_signal}),
    signal_brush = reactive({input$brush})
    )
}

# helpers ---------
gen_signal_plot <-
  function(data, signal){
    data %>%
      ggplot(aes(x = t)) +
      geom_line(aes_string(y = signal))
  }
