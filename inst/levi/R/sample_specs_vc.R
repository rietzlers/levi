sample_specs_view <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("alloy_name"), label = "Alloy"),
    numericInput(ns("mass"), label = "Sample-Mass [g]", value = 1.29224),
    numericInput(ns("radius"), label = "Sphere-Radius [mm]", value = 6.528/2)
  )
}


sample_specs_ctrl <- function(input, output, session){

  # return-Values ------
  reactive({
    list(
      alloy = input$alloy_name,
      mass = input$mass,
      radius = input$radius
      )
  })
}
