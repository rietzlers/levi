## inputs.R ##

boxed_numericInput <-
  function(id, label = id, value = NULL, width = 12){
    numericInput(id, label = label, value = value)
  }

sample_specs <-
  tibble::tribble(
    ~ id, ~ label,
    "frame_rate" ,  "Frame-Rate [Hz]",
    "sample_mass", "Sample-Mass [g]",
    "sphere_radius",  "Sphere-Radius [mn]"
    )

sample_spec_inputs <-
  sample_specs %>%
  purrr::pmap(boxed_numericInput)
