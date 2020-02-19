# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(
    plotlyOutput(ns("spectrum_plot"), height = 250),
    uiOutput(ns("spectrum_info")),
    selectInput(ns("type"), label = "", selected = "raw", choices = c("raw", "log")),
    fluidRow(
      column(width = 4, numericInput(ns("bp_low"),  label = "", value = 0, min = 0, step = 1)),
      column(width = 4, numericInput(ns("bp_high"), label = "", value = 200))
    ),
    verbatimTextOutput(ns("select_info")),
    verbatimTextOutput(ns("click_info"))
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_data, signal_name, frame_rate){

  ns <- session$ns

  est_spec <- reactive({
    estimate_signal_spectrum(tevi_data(), signal_name(), frame_rate())
  })

  output$spectrum_plot <-
    renderPlotly({
      spec_plot(est_spec(), type = input$type) %>%
        ggplotly(source = ns("spectrum_plotly")) %>%
        layout(dragmode = "select") %>%
        event_register("plotly_brushed") %>%
        event_register("plotly_selected")
    })

  output$spectrum_info <- renderUI({
    c(f, ...)  %<-% levi::get_dom_freq(est_spec())
    h5(str_glue("Maximum Freq.: {round(f, 2)} Hz"))
  })

  output$select_info <- renderPrint({
    event_data("plotly_brushed", source = ns("spectrum_plotly"))
    })

  output$click_info <- renderPrint({
    event_data("plotly_click", source = ns("spectrum_plotly"))
  })

}

# helper-functions ----------
spec_plot <- function(est_spec, type = "raw"){
  ylab = "raw"
  if( type == "log"){
    ylab = "log"
    est_spec <- est_spec %>% mutate(spec = log(spec))
  }

    est_spec %>%
    ggplot(aes(x = f, y = spec)) +
    geom_line() +
    labs(x = "Frequency [Hz]",
         y = ylab)
}

estimate_signal_spectrum <- function(signal_data, signal_name, frame_rate, type = "spectrum") {
  if(type == "spectrum"){
    est_spec <-
      spectrum(
        ts(signal_data[[signal_name]], frequency = frame_rate),
        plot = FALSE)

    return(tibble(f = est_spec$freq,  spec = est_spec$spec, fc_amp = sqrt(spec / length(f))))
  }

  if(type == "fft"){
    levi::fftc(signal_data, signal_name, sr = frame_rate)
  }

}


