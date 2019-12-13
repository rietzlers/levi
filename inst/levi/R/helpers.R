# helpers.R #

get_brush_range <-
  function(brush){
    validate(need(brush, "brush range"))
    c(xmin, xmax, ...rest)  %<-% brush
    c(xmin, xmax)
  }
