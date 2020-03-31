
sample_specs_table <-  read_excel(file.path("./www/sample_specs", "Samples-Database.xlsx"))

sample_specs_view <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12, title = "Select alloy by clicking on table-row",
        DT::dataTableOutput(ns("sample_specs_DT")))
  )
}


sample_specs_ctrl <- function(input, output, session){

  # module-data -------------
  sample_specs <- reactiveVal()
  observeEvent(input$sample_specs_DT_rows_selected,{
    sample_specs(sample_specs_table[input$sample_specs_DT_rows_selected, ])
  })

  # bookmark-observers ----------
  onBookmark(function(state){
    state$values$sample_specs <- sample_specs()
  })
  onRestore(function(state){
    sample_specs(state$values$sample_specs)
  })

  output$sample_specs_DT <- DT::renderDataTable({
    validate(need(sample_specs_table, label = "sample-specs data"))
    sample_specs_table
    },
    rownames = FALSE,
    selection = 'single',
    server = TRUE, filter = 'top',
    extensions = c('Buttons', 'Responsive', 'FixedColumns'),
    options = list(
      dom = 'Bftlip',
      buttons = c('csv', 'excel', 'pdf'),
      pageLength = 25,
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 2)
    ))

  # return-Values ------
  reactive({
    validate(need(sample_specs(), message = "Alloy/Sample-Specifications are missing."))
    c(alloy_name, . , d, m, Temp_liquid) %<-% sample_specs()
    list(
      alloy = alloy_name,
      mass = m,
      radius = d/2,
      Temp_liquid = Temp_liquid
      )
  })
}
