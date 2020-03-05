# parameters.R ##


model_params_view <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, collapsible = TRUE,
        title = "Set parameters",
        tibble::tribble(
          ~ id, ~ label,
          "frame_rate" , "Frame-Rate [Hz]",
          "sample_mass", "Sample-Mass [g]",
          "sphere_radius", "Sphere-Radius [mn]"
        ) %>%
          purrr::pmap( ~ numericInput(inputId = ns(.x), label = .y, value = NULL))
    ),
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
