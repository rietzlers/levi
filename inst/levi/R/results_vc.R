
resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, plotlyOutput(ns("spec_analysis_results_display"))),
      column(width = 6, DT::dataTableOutput(ns("spec_analsis_results_DT")))
    )
  )
}


results_ctrl <- function(input, output, session, spec_analysis_results, bp){

  output$spec_analysis_results_display <-
    renderPlotly({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        ggplot(aes(x = t, color = type)) +
        geom_point(aes(y = dom_freq), shape = "x", size = 2) +
        geom_point(aes(y = f0), size = 2) +
        ylim(bp())
    })

  output$spec_analsis_results_DT <-
    DT::renderDataTable({
      validate(need(spec_analysis_results(), label = "spec_analysis_results"))
      spec_analysis_results() %>%
        arrange(type, t)
    },
    server = TRUE)
}
