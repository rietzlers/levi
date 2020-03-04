# signal_vc.R #

# view ------------
signalUI <- function(id, height = 150){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4, selectInput(ns("selected_signal"), label = NULL, choices = NULL))
    ),
    plotOutput(
      ns("signal"), height = height,
      brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))
    )
}

# controller ----------
signal_ctrl <- function(input, output, session, data, variable = NULL, bp){

  observeEvent(data(),{
        updateSelectInput(session, "selected_signal", choices = names(data()), selected = variable)
        }) #update signal-selection
  output$signal <- renderPlot({
    validate(need(input$selected_signal, label = "signal"))
    gen_signal_plot(data(), input$selected_signal)
    })

   # return-values ---------
  list(
    signal_name  = reactive({input$selected_signal}),
    signal_brush = reactive({input$brush})
    )
}

# helpers ---------
gen_signal_plot <-
  function(data, signal){
    graph <-
      data %>%
      ggplot(aes(x = t)) +
      geom_line(aes_string(y = signal))

    if(signal == "pyro_temp"){
      data <-
        data %>%
        add_temperature()
      graph <-
        graph +
        geom_line(data = data %>% add_temperature(),
                  aes(x = t, y = smoothed_temp), linetype = "dashed", color = "red")
    }
    graph
  }
