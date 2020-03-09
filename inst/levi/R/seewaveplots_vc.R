seewave_view <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("spec_and_osc_plot"), height = "800px")
  )
}

seewave_ctrl <- function(input, output, session, tevi_model, selected_signal, selectedSidebarMenu){

  output$spec_and_osc_plot <- renderPlot({
    signal <- tevi_model()$tevi_data[[selected_signal()]]
    signal <- signal - mean(signal, na.rm = TRUE)
    seewave::spectro(
      signal, f = tevi_model()$frame_rate, wl = 2^8, ovlp = 50, flim = c(20, 70)/1000,
      osc = TRUE, alab = selected_signal()
    )
  })
}
