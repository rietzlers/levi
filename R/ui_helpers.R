# helpers.R #

#' convert brush from plotOutput to numeric vector
#'
#' @param brush input-object
#' @param failure_massage string: message to display in case there is no brush
#'
#' @return numeric vector with the range of the brushed area
#' @export
get_brush_range <-
  function(brush, failure_massage = "select range by brushing plot"){
    validate(need(brush, failure_massage))
    c(xmin, xmax, ...rest)  %<-% brush
    c(xmin, xmax)
  }
