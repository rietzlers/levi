## helpers.R ##


select_data <- function(session, data, brush_data) {
  selected_data <-
    brushedPoints(data, brush_data, xvar = "time")
  validate(need(
    nrow(selected_data) > 0,
    "select data by brushing (left-click and pull) over signal-plot"
  ))
  selected_data
}


