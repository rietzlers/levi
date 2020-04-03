# spectrum_vc.R ##

# view -----------
spectrumUI <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Spectrogram",
        fluidRow(
          column(width = 6,
                 plotOutput(ns("complete_spectrum"), height = 250,
                            brush = brushOpts(id = ns("brush"), fill = "#ccc", direction = "x", resetOnNew = FALSE))),
          column(width = 6, plotlyOutput(ns("bp_spectrum"), height = 250))
        )
      ),
      tabPanel("Controls",
               selectInput(ns("scale"), label = "Set scaling of spectrogram ordinate", selected = "log10", choices = c("raw", "log10")),
               bsTooltip(ns("scale"), "scaling of spectrogram-ordinate", "top"),
               selectInput(ns("type"), label = "Choose type of data to fit lorentz to", selected = "spectrum", choices = c("spectrum", "fft")),
               bsTooltip(ns("type"), "use FFT/spectrogram to calculate periodogram", "top"),
               textInput(ns("spans"), label = "span", value = "c(3,3)"),
               bsTooltip(ns("spans"), "specify daniell-smoother: NULL for no smoothing", "top", options = list(container = "body")),
               numericInput(ns("taper"), label = "taper", value = 0.1, step = .1, min = 0, max = 1),
               bsTooltip(ns("taper"), "apply cosine-taper to % of window", "top"))
    )
  )
}

# controller ------------
spectrum_ctrl <- function(input, output, session, tevi_model, data_selection, signal_name,
                          selected_tab, spectrum_view_UI, tasks, notifications){

  # data ----
  est_spec <- reactive({
    levi::estimate_signal_spectrum(
      data_selection(),
      signal_name(),
      tevi_model()$frame_rate,
      spans = spans(),
      taper = taper()
    )
  })
  lfit <- reactive({
    lfit <-
      levi::fit_lorentz(
      dplyr::filter(est_spec(), type == input$type),
      bp = bp(), # lorentz-kurve wird IMMER an die BP-gefilterte Kurve angepasst!
      sr = tevi_model()$frame_rate)
    notifications_list <- notifications()
    if(is.null(lfit)){
      notifications_list[["lfit"]] <- notificationItem("lorentz-fit did not succed", icon = icon("frown"), status = "warning")
    }else{
      notifications_list[["lfit"]] <- NULL
    }
    notifications(notifications_list)
    lfit
  })
  bp <- reactive(({
    input$brush %>%
      levi::get_brush_range( "set band-pass by brushing spectrum-plot") %>%
      round(1)
  }))
  spans <- reactive({
    spans <- -1
    tryCatch(
      error = function(e) e,
      spans <- eval(rlang::parse_expr(input$spans))
    )
    validate(need(min(spans) > 1,
                  message = "No valid spans for Daniell-Smoother! It must be a numeric vector of positve integers, i.e. c(3,3), c(2)"))
    spans
  })
  taper <- reactive({
    validate(need(input$taper, label = "taper"))
    input$taper
  })

  dom_freq <- reactive({
    f_dom <- NULL
    c(f_dom, ...)  %<-%
      (est_spec() %>%
         dplyr::filter(type == input$type, f %>% between(bp()[1], bp()[2])) %>%
         levi::get_dom_freq(sample_rate = tevi_model()$frame_rate) %>%
        round(2))
    validate(need(f_dom, "f_dom"))
    f_dom
  })
  f0 <- reactive({
    validate(need(lfit(), message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit())[2]
  })
  d <- reactive({
    validate(need(lfit(), message = "lorentz-fit did not succed"))
    lorentz_parameters(lfit())[3]
  })

  # outputs  -----------
  output$complete_spectrum <- renderPlot({
      gen_spec_plot(
        est_spec(),
        lfit = NULL,
        scale = input$scale,
        bp = c(0, tevi_model()$frame_rate/2),
        sample_rate = tevi_model()$frame_rate,
        type_choosen= input$type
        )
    })
  output$bp_spectrum <- renderPlotly({
      gen_spec_plot(
        est_spec(),
        lfit = lfit(),
        scale = input$scale,
        bp = bp(),
        sample_rate = tevi_model()$frame_rate,
        type_choosen =  input$type,
        plot_type = "plot_ly"
      )
    })

  # return-values -----------
  list(
    type = reactive(input$type),
    bp = bp,
    dom_freq = dom_freq,
    f0 = f0,
    d = d,
    spans = reactive(input$spans),
    taper = reactive(input$taper)
  )

}





