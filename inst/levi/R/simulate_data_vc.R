simulate_data_view <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #column(width = 1, actionButton(ns("resample"), "resample")),
      column(width = 5, textInput(ns("signal"), "Signal", width = "100%", value = "100 * exp(-.5 * t) * (sin(2 * pi * (30 - (0.5)*t) * t))")),
      column(width = 2, numericInput(ns("noise"), "Noise-SD:", value = 30, min = 0, max = 100, step = 1)),
      column(width = 2, numericInput(ns("sr"), "Sample-Rate", value = 400, min = 0, max = 400)),
      column(width = 2, numericInput(ns("T"), "Observation-Time", value = 5, min = 0, max = 10))
    ),
    fluidRow(
      column(width = 2, HTML("choose data to analyse:")),
      column(width = 4, selectInput(ns("data_choice"), label = NULL, choices = c("Tevi-Data" = "tevi_data", "Simulated Data" = "sim_data"))),
    ),
    plotOutput(ns("signal_plot"), height = "250px"),
    plotOutput(ns("spec_osc")),
    fluidRow(
      column(width = 4, numericInput(ns("lp"), "Lower BP", min = 0, max = 400, value = 0)),
      column(width = 4, numericInput(ns("hp"), "Upper BP", min = 0, max = 400, value = 400))
    )
  )
}


simulate_data_ctrl <- function(input, output, session, resample_UI, selected_sidebar_tab){

  observeEvent({
    selected_sidebar_tab()
    input$data_choice
    },
               {
                 if(input$data_choice == "sim_data"){
                   resample_UI(actionButton(session$ns("resample"), "resample"))
                 }else{
                   resample_UI(NULL)
                 }
               })
  observeEvent(input$sr, updateNumericInput(session, "hp", value = input$sr/2, max = input$sr/2))
  ex_data <- reactive({
    input$resample
    example_data <- NULL
    tryCatch(
      error = function(e) {
        e
      },
      {
      example_data <- gen_example_data(
        T = input$T,
        sr = input$sr,
        signal = input$signal,
        noise_sd = input$noise
      )
      }
    )
    validate(need(example_data, message = "can not simulate data; Probably you misspecified the function-term"))
    example_data
  })

  output$signal_plot <- renderPlot({
    input$resample
    sr <- input$sr
    ex_data <- ex_data()

    sig_plot <-
      ex_data %>%
      ggplot(aes(x = t, y = s)) + geom_line()

    # calculate fc
    sig_fc <- levi::fftc(ex_data, "s", sr)
    # max amplitude
    c(f_max, fmax_amp) %<-% (get_dom_freq(sig_fc, sr) %>% round(1))

    # bp filter
    bp_signal <-
      levi::bp_filter(ex_data, "s", bp = c(input$lp, input$hp), sr)

    sig_plot <-
      sig_plot +
      geom_line(data = bp_signal, aes(x = t, y = s), color = "red") +
      labs(
        title = str_glue("Signal (black) and BP-Filted Signal (red); Dom. Freq: {f_max} Hz with Amplitude: {fmax_amp}")
      )

    sig_plot

  })

  output$spec_osc <- renderPlot({
    spectro(ex_data()$s, f=input$sr, wl = nextn(floor(input$sr), factors = c(2))/4, ovlp = 50, osc = TRUE,
            flim = c(input$lp, input$hp - 1)/1000)
  })

  return(
    list(
    reactive(
      list(
        tevi_data = ex_data(),
        tevi_data_name = "Simulated data",
        exp_time_range = c(0, input$T),
        HPs = NULL,
        frame_rate = input$sr
      )
    ),
    reactive(input$data_choice)
    )
  )
}
