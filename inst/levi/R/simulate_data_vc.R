simulate_data_view <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #column(width = 1, actionButton(ns("resample"), "resample")),
      column(width = 5, textInput(ns("signal"), "Specify the deterministic part of the signal", width = "100%", value = "100 * exp(-.5 * t) * (sin(2 * pi * (30 - (0.5)*t) * t))")),
      column(width = 2, numericInput(ns("noise"), "Add white noise with SD:", value = 30, min = 0, max = 100, step = 1)),
      column(width = 2, numericInput(ns("sr"), "Sample-Rate", value = 400, min = 0, max = 400)),
      column(width = 2, numericInput(ns("T"), "Observation-Time", value = 5, min = 0, max = 10))
    ),
    fluidRow(
      column(width = 4, selectInput(ns("data_choice"), label = "Choose Analysis Data", choices = c("Tevi-Data" = "tevi_data", "Simulated Data" = "sim_data"))),
      column(width = 3, numericInput(ns("lp"), "Lower BP", min = 0, max = 400, value = 0)),
      column(width = 3, numericInput(ns("hp"), "Upper BP", min = 0, max = 400, value = 400))
    ),
    plotOutput(ns("signal_plot"), height = "250px"),
    plotOutput(ns("spec_osc"))
  )
}


simulate_data_ctrl <- function(input, output, session, resample_UI){

  # local data ------
  signal_function_string <- reactive(input$signal) %>% debounce(1000)
  noise_sd <- reactive(input$noise) %>% debounce(500)
  sample_rate <- reactive(input$sr) %>% debounce(500)
  T <- reactive(input$T) %>% debounce(500)

  ex_data <- reactive({
    input$resample
    example_data <- NULL
    tryCatch(
      error = function(e) {e},
      {
        example_data <-
          gen_example_data(
            T = T(),
            sr = sample_rate(),
            signal = signal_function_string(),
            noise_sd = noise_sd()
          ) %>%
          mutate(radius_y = s)
      }
    )
    validate(
      need(example_data, message = "can not simulate data; Probably you misspecified the function-term")
    )
    example_data
  })

  # observers ---------
  observeEvent({input$data_choice},{
    if (input$data_choice == "sim_data") {
      resample_UI(actionButton(session$ns("resample"), "repeat data-simulation"))
    } else{
      resample_UI(NULL)
    }})
  observeEvent(input$sr, updateNumericInput(session, "hp", value = input$sr/2, max = input$sr/2))

  # output-ctrls -------------
  output$signal_plot <- renderPlot({
    input$resample
    sr <- sample_rate()
    ex_data <- ex_data()

    sig_plot <-
      ex_data %>%
      ggplot(aes(x = t, y = s)) + geom_line()

    # calculate fc
    sig_fc <- levi::fftc(ex_data, "s", sr)
    # max amplitude
    c(f_max, fmax_amp) %<-% (get_dom_freq(sig_fc) %>% round(1))

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
    spectro(
      ex_data()$s,
      f = sample_rate(),
      wl = nextn(floor(sample_rate()), factors = c(2))/4,
      ovlp = 50,
      osc = TRUE,
      flim = c(input$lp, input$hp - 1)/1000)
  })

  # return-values ----------
  return(
    list(
      reactive(
        list(
          analysis_data = ex_data(),
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
