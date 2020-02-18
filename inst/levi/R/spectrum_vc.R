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
spectrum_ctrl <- function(input, output, session, data, signal, frame_rate){

  ns <- session$ns

  est_spec <- reactive({
    estimate_signal_spectrum(data(), signal(), frame_rate())
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
    max_freq <- get_freq_max(est_spec())
    h5(str_glue("Maximum Freq.: {round(max_freq$max_freq, 2)} Hz"))
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
    ggplot(aes(x = freq, y = spec)) +
    geom_line() +
    labs(x = "Frequency [Hz]",
         y = ylab)
}

estimate_signal_spectrum <- function(data, signal, frame_rate) {
  est_spec <-
    spectrum(
      ts(data  %>% pull(!!signal), frequency = frame_rate),
      plot = FALSE)

  tibble(freq = est_spec$freq, spec = est_spec$spec)
}

get_freq_max <- function(est_spec){
    est_spec %>%
    filter(near(spec, max(spec), tol = 0.5)) %>%
    summarize(max_freq = mean(freq))
}
