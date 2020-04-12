
dashboard_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sample_and_exp_info")),
    tabsetPanel(
      tabPanel("Experiment- and Alloy-Specification", sample_specs_view(ns("sample_specs")), icon = icon("wpexplorer")),
      tabPanel("Upload .csv-data from Tevi", load_tevi_data_UI(ns("load_tevi_data")), icon = icon("upload")),
      tabPanel("Report-Notes", report_notes_UI(ns("report_notes")), icon = icon("clipboard"))
    )
  )
}

dashboard_ctrl <- function(input, output, session){
  # render-output ------
  output$sample_and_exp_info <- renderUI({
    c(alloy_name, m, radius, Temp_liquid) %<-% sample_specs()
    box(width = 12,
        HTML(
          str_glue("<b>Selected Alloy:</b> {alloy_name}
               </br><b>Sample-Specifications:</b> Mass of sample: {m} g; Diameter of Sample: {radius*2} mm; Liquidus-Temp: {Temp_liquid} K,
               </br> <b>Frame-Rate of camera:</b> {tevi_model()$frame_rate} Hz
               </br> <b>Tevi-Data:</b> {tevi_model()$tevi_data_name}")
        ))
  })

  tevi_model <-  callModule(load_tevi_data_ctrl, "load_tevi_data")
  sample_specs <- callModule(sample_specs_ctrl, "sample_specs")
  callModule(report_notes_ctrl, "report_notes", sample_specs)

  list(
    tevi_model,
    sample_specs
  )
}
