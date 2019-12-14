# parameters.R ##


parametersUI <- function(id) {
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


parameters <- function(input, output, session, raw_tevi_data){

  # update ui ------------
  observeEvent(
    raw_tevi_data(),
    {
      # estimate sample/frame-rate from mean dt
      c(est_sample_freq) %<-%
        (raw_tevi_data() %>%
           summarize(est_sample_freq = round(1 / mean(diff(time), na.rm = TRUE))))

      updateNumericInput(session, "frame_rate", value = est_sample_freq)

    })

  list(reactive(input$frame_rate))
  }
