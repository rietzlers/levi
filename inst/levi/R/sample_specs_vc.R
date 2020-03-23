sample_specs_view <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12, title = "Select alloy by clicking on table-row",
        DT::dataTableOutput(ns("sample_specs_DT")))
  )
}


sample_specs_ctrl <- function(input, output, session, sample_spec_info_UI, selected_tab, tasks, notifications){

  sample_specs <-  read_excel(file.path("./www/sample_specs", "Samples-Database.xlsx"))

  selected_alloy <- reactive({
    validate(need(input$sample_specs_DT_rows_selected, message = "Select Alloy-Spec in tab 'Set up sample specs'"))
    sample_specs[input$sample_specs_DT_rows_selected, ]
  })



  output$sample_specs_DT <- DT::renderDataTable({
    validate(need(sample_specs, label = "sample-specs data"))
    sample_specs
    },
    selection = 'single',
    server = TRUE, filter = 'top', extensions = c('Buttons'),
    options = list(
      dom = 'Bftlip',
      buttons = c('csv', 'excel', 'pdf'),
      pageLength = 25,
      autoWidth = TRUE
    ))

  # return-Values ------
  reactive({
    validate(need(selected_alloy(), message = "Select alloy-specs in tab 'Set up sample specs'"))
    c(alloy_name, . , d, m, Temp_liquid) %<-% selected_alloy()
    list(
      alloy = alloy_name,
      mass = m,
      radius = d/2,
      Temp_liquid = Temp_liquid
      )
  })
}
