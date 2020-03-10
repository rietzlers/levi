sample_specs_view <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("sample_specs_DT"))
  )
}


sample_specs_ctrl <- function(input, output, session, sample_spec_info_UI, selected_tab){

  sample_specs <-  read_excel(file.path("www/sample_specs", "Samples-Database.xlsx"))

  output$sample_specs_DT <- DT::renderDataTable({
    validate(need(sample_specs, label = "sample-specs data"))
    sample_specs
    },
    server = TRUE, filter = 'top', extensions = c('Buttons'),
    options = list(
      dom = 'Bftlip',
      buttons = c('csv', 'excel', 'pdf'),
      pageLength = 25,
      autoWidth = TRUE
    ))

  observeEvent(selected_tab(),{
    if (!(selected_tab() %in% c("spec_osc", "spec_dom_freq", "inst_freqs", "sig_envelope"))){
      sample_spec_info_UI(
      div(
        textInput(session$ns("alloy_name"), label = "Alloy"),
        numericInput(session$ns("mass"), label = "Sample-Mass [g]", value = 1.29224),
        numericInput(session$ns("radius"), label = "Sphere-Radius [mm]", value = 6.528/2)
      ))
    }else{sample_spec_info_UI(NULL)}
  })
  # return-Values ------
  reactive({
    list(
      alloy = input$alloy_name,
      mass = input$mass,
      radius = input$radius
      )
  })
}
