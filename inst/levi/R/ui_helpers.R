# helpers.R #

get_brush_range <-
  function(brush, failure_massage = "select range by brushing plot"){
    validate(need(brush, failure_massage))
    c(xmin, xmax, ...rest)  %<-% brush
    c(xmin, xmax)
  }
