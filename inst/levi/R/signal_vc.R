# signal_vc.R #

signalUI <- function(id, height = 200){

  ns <- NS(id)

  tagList(
    selectInput(ns("selected_signal"), label = "select signal", choices = NULL),
    plotOutput(
      ns("signal"), height = height,
      brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x")
      ),
    verbatimTextOutput(ns("brush_info"))
  )
}


signal <- function(input, output, session, data){

  observeEvent(
    data(),
    {
      updateSelectInput(session, "selected_signal", choices = names(data()))
    }
    )

  output$signal <-
    renderPlot({gen_signal_plot(data(), input$selected_signal)})

  output$brush_info <-
    renderPrint(get_brush_range(input$brush))

  return(reactive({get_brush_range(input$brush)}))

}

gen_signal_plot <-
  function(data, signal){
    data %>%
      ggplot(aes(x = time)) +
      geom_line(aes_string(y = signal))
  }
