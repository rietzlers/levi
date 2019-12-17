# signal_vc.R #

signalUI <- function(id, height = 200){
  ns <- NS(id)

  tagList(
    selectInput(ns("selected_signal"), label = "select signal", choices = NULL),
    plotOutput(
      ns("signal"), height = height,
      brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = TRUE))
    )
}

signal <- function(input, output, session, data, variable = NULL){

  observeEvent(data(),
      updateSelectInput(session, "selected_signal", choices = names(data()), selected = variable))

  output$signal <-
    renderPlot({gen_signal_plot(data(), input$selected_signal)})

  list(
    signal =
      reactive({
        validate(need(input$selected_signal, "choose signal"))
        rlang::sym(input$selected_signal)}),
    brush =
      reactive(input$brush)
    )
}

gen_signal_plot <-
  function(data, signal){
    data %>%
      ggplot(aes(x = time)) +
      geom_line(aes_string(y = signal))
  }
