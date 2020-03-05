# parameters.R ##


model_params_view <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, numericInput(ns("frame_rate"), label = "Frame Rate [Hz]", value = NULL)),
      column(width = 4, numericInput(ns("sample_mass"), label = "Sample-Mass [g]", value = 1.29224)),
      column(width = 4, numericInput(ns("sphere_radius"), label = "Sphere-Radius [mm]", value = 6.528/2))
    )
  )
}


model_params_ctrl <- function(input, output, session, data){

  # update ui ------------
  observeEvent(
    data(),
    {
      # estimate sample/frame-rate from mean dt
      c(est_sample_freq) %<-%
        (data() %>%
           summarize(est_sample_freq = round(1 / mean(diff(t), na.rm = TRUE))))

      updateNumericInput(session, "frame_rate", value = est_sample_freq)

    })

  # return values ---------
  list(
    frame_rate = reactive(input$frame_rate),
    mass = reactive({
      validate(need(input$sample_mass, label = "sample mass"))
      input$sample_mass}),
    radius = reactive({
      validate(need(input$sphere_radius, label = "sphere radius"))
      input$sphere_radius})
    )
  }
