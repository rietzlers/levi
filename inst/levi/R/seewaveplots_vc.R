seewave_view <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3, selectInput(ns("selected_signal"), label = "Signal", choices = NULL)),
      column(width = 2, numericInput(ns("bp_min"), label = "BP-Min", value = 0, min = 0, step = 5)),
      column(width = 2, numericInput(ns("bp_max"), label = "BP-Max", value = 100, min = 0, max = 200, step = 5)),
      column(width = 2, numericInput(ns("ws"), label = "window-start", value = 0, min = 0, step = 1)),
      column(width = 2, numericInput(ns("we"), label = "window-end", value = 10, min = 0, step = 1))
    ),
    plotOutput(ns("seewave_plot"), height = "800px")
  )
}

seewave_ctrl <- function(input, output, session, tevi_model, selected_sidebar_tab){

  # observers -----------
  observeEvent(tevi_model(),{
    updateSelectInput(session, "selected_signal", label = "Signal",
                      choices = names(tevi_model()$tevi_data),
                      selected = "radius_y")
  }) #update signal-selection

  output$seewave_plot <- renderPlot({
    sig_name <- input$selected_signal
    time_range <- c(input$ws, input$we)
    sr <- tevi_model()$frame_rate
    bp <- c(input$bp_min, input$bp_max)
    bp[1] <- max(0, bp[1])
    bp[2] <- min(sr, bp[2])

    data_selection <- tevi_model()$tevi_data %>% filter(t %>% between(time_range[1], time_range[2]))
    t <- data_selection[["t"]]
    signal <- data_selection[[sig_name]]
    signal <- signal - mean(signal, na.rm = TRUE)


    N <- length(signal) # Nr. samples T = N * sample_rate
    T <- N / sr # Observation-length

    wl <- 2^8
    #to do: select window-length conditional on Observation-length

    if(selected_sidebar_tab() == "spec_osc"){
      validate(need(N > 2^8, message = str_glue("selected time range is to short (it is only {round(T, 2)}s long)")))
      return(
        seewave::spectro(
        signal, f = sr, wl = wl, ovlp = 50, flim = bp/1000,
        osc = TRUE, alab = sig_name
      )
     )
    }
    if(selected_sidebar_tab() == "spec_dom_freq"){
      validate(need(N > 2^8, message = str_glue("selected time range is to short (it is only {round(T, 2)} s long)")))
      dom_freqs <- seewave::dfreq(signal, f = sr, wl = wl, ovlp = 50, bandpass = bp, threshold = 10, plot = FALSE)
        return(
          {
            seewave::spectro(signal, f = sr, wl = wl, ovlp = 50, flim = bp/1000)
            points(dom_freqs, col = "red", bg = "yellow", pch = 21)
          }
        )
    }
    if(selected_sidebar_tab() == "inst_freqs"){
      return(
        seewave::ifreq(signal, f = sr, threshold = 10,  ylim = bp/1000,
                       main="Instantaneous frequency with Hilbert transform")
      )
    }
    if(selected_sidebar_tab() == "sig_envelope"){
      return({
        seewave::oscillo(signal, f = sr, alab = sig_name)
        par(new = TRUE)
        seewave::env(signal, f = sr, envt = "hil", msmooth = c(20, 0), alab = "", colwave=2)
        legend("topleft", y=1,"smoothed envelope (hilbert)", col=2, lty=1, lwd=2, bty="n")
      })
    }
  })
}
