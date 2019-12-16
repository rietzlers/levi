# signal_vc.R #

signalUI <- function(id, height = 200){

  ns <- NS(id)

  tagList(
    selectInput(ns("selected_signal"), label = "select signal", choices = NULL),
    plotOutput(
      ns("signal"), height = height,
      brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = TRUE)
      )
  )
}


signal <- function(input, output, session, data, variable = NULL){

  observeEvent(
    data(),
    {
      updateSelectInput(session, "selected_signal", choices = names(data()))
      if(!is.null(variable)){
        updateSelectInput(session, "selected_signal", selected = variable)
      }
    }
    )

  output$signal <-
    renderPlot({gen_signal_plot(data(), input$selected_signal)})

  return(reactive({get_brush_range(input$brush)}))
}

gen_signal_plot <-
  function(data, signal){
    data %>%
      ggplot(aes(x = time)) +
      geom_line(aes_string(y = signal))
  }
