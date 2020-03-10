seewave_view <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("seewave_plot"), height = "800px")
  )
}

seewave_ctrl <- function(input, output, session, tevi_model, signal_selections, selectedSidebarMenu){

  output$seewave_plot <- renderPlot({
    sig_name <- signal_selections()$selected_signal
    time_range <- signal_selections()$time_range
    bp <- signal_selections()$bp

    data_selection <- tevi_model()$tevi_data %>% filter(t %>% between(time_range[1], time_range[2]))
    t <- data_selection[["t"]]
    signal <- data_selection[[sig_name]]
    signal <- signal - mean(signal, na.rm = TRUE)
    sr <- tevi_model()$frame_rate

    N <- length(signal) # Nr. samples T = N * sample_rate
    T <- N / sr # Observation-length

    wl <- 2^8
    #to do: select window-length conditional on Observation-length

    if(selectedSidebarMenu() == "spec_osc"){
      validate(need(N > 2^8, message = str_glue("selected time range is to short (it is only {round(T, 2)}s long)")))
      return(
        seewave::spectro(
        signal, f = sr, wl = wl, ovlp = 50, flim = bp/1000,
        osc = TRUE, alab = sig_name
      )
     )
    }
    if(selectedSidebarMenu() == "spec_dom_freq"){
      validate(need(N > 2^8, message = str_glue("selected time range is to short (it is only {round(T, 2)} s long)")))
      dom_freqs <- seewave::dfreq(signal, f = sr, wl = wl, ovlp = 50, bandpass = bp, threshold = 10, plot = FALSE)
      print(dom_freqs)
        return(
          {
            seewave::spectro(signal, f = sr, wl = wl, ovlp = 50, flim = bp/1000)
            points(dom_freqs, col = "red", bg = "yellow", pch = 21)
          }
        )
    }
    if(selectedSidebarMenu() == "inst_freqs"){
      return(
        seewave::ifreq(signal, f = sr, threshold = 10, col="darkviolet", ylim = bp/1000,
                       main="Instantaneous frequency with Hilbert transform")
      )
    }
    if(selectedSidebarMenu() == "sig_envelope"){
      return({
        seewave::oscillo(signal, f = sr, alab = sig_name)
        par(new = TRUE)
        seewave::env(signal, f = sr, envt = "hil", msmooth = c(20, 0), alab = "", colwave=2)
        legend("topleft", y=1,"smoothed envelope (hilbert)", col=2, lty=1, lwd=2, bty="n")
      })
    }
  })
}
