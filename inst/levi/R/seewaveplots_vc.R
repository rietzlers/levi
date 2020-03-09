seewave_view <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("seewave_plot"), height = "800px")
  )
}

seewave_ctrl <- function(input, output, session, tevi_model, selected_signal, selectedSidebarMenu){

  output$seewave_plot <- renderPlot({
    t <- tevi_model()$tevi_data[["t"]]
    signal <- tevi_model()$tevi_data[[selected_signal()]]
    signal <- signal - mean(signal, na.rm = TRUE)
    sr <- tevi_model()$frame_rate
    sig_name <- selected_signal()

    if(selectedSidebarMenu() == "spec_osc"){
      return(
        seewave::spectro(
        signal, f = sr, wl = 2^8, ovlp = 50, flim = c(20, 70)/1000,
        osc = TRUE, alab = sig_name
      )
     )
    }
    if(selectedSidebarMenu() == "spec_dom_freq"){
      dom_freqs <- seewave::dfreq(signal, f = sr, wl = 2^8, ovlp = 50, bandpass = c(5, 50), threshold = 10, plot = FALSE)
        return(
          {
            seewave::spectro(signal, f = sr, wl = 2^8, ovlp = 50, flim = c(25, 50)/1000)
            points(dom_freqs, col = "red", bg = "yellow", pch = 21)
          }
        )
    }
    if(selectedSidebarMenu() == "inst_freqs"){
      return(
        seewave::ifreq(signal, f = sr, threshold = 10, col="darkviolet", main="Instantaneous frequency with Hilbert transform")
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
