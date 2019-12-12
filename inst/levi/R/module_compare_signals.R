# module_compare_signals.R #

compareSignalsUI <- function(id){

  ns <- NS(id)

  tagList(
    signalPlotUI(ns("signal1"), width = 6),
    signalPlotUI(ns("signal2"), width = 6)
  )
}

compareSignals <- function(input, output, session, raw_tevi_data, frame_rate){


  callModule(signalPlot, "signal1", reactive(raw_tevi_data()), reactive(frame_rate()))

  callModule(signalPlot, "signal2", reactive(raw_tevi_data()), reactive(frame_rate()))

}
