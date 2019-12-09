

server <-
  function(input, output, session) {
    # reactive data ----
    raw_tevi_data <-
      reactive({
        validate(need(input$file, label = "tevi-data (.dat-file)"))

        df_raw <-
          vroom::vroom(
            input$file$datapath,
            delim = "\t",
            col_types = cols(
              `Absolute Time` = col_time(format = "%H:%M:%OS"),
              .default = col_double()
            ),
            trim_ws = TRUE
          ) %>%
          janitor::clean_names()


        df <-
          df_raw[-1,] %>%
          arrange(seconds) %>%
          mutate(time = seconds - seconds[1])

        names(df) <-
          map_chr(names(df), function(name) {
            str_remove(name, "^a_|^r_")
          })

        validate(need(nrow(df) > 0, "there are no observations in uploaded data"))

        # update UI -
        # available signals
        col_names <- df %>% names()
        signal_names <- col_names[col_names != "time"]
        updateSelectInput(
          session,
          "signal_choice",
          choices = signal_names,
          selected = sample(signal_names, size = 1)
        )
        # sample/frame-rate
        sample_rate <-
          df %>% summarize(est_sample_freq = round(1 / mean(diff(time), na.rm = TRUE)))
        updateNumericInput(session, "frame_rate", value = sample_rate$est_sample_freq)

        df
      })

    data_selection <-
      reactive({
        selected_data <-
          brushedPoints(raw_tevi_data(), input$signal_plot_brush, xvar = "time")
        validate(need(
          nrow(selected_data) > 0,
          "select data by brushing (left-click and pull) over signal-plot"
        ))
        selected_data
      })

    signal <-
      reactive({
        rlang::sym(req(input$signal_choice))
      })

    est_spec <-
      reactive({
        est_spec <-
          spectrum(ts(
            data_selection() %>%
              pull(!!signal()),
            frequency = input$frame_rate
          ),
          plot = FALSE)

        tibble(freq = est_spec$freq, spec = log(est_spec$spec))
      })

    # dashboard  -----
    output$raw_tevi_data_table <-
      renderPrint({
        raw_tevi_data()
      })

    output$plot_center_xy <-
      renderPlot({
        raw_tevi_data() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = center_x)) +
          geom_line(aes(y = center_y)) +
          labs(y = "Center-Coordinates [px]")
      })

    output$plot_temp <-
      renderPlot({
        raw_tevi_data() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = pyro_temp))
      })

    output$plot_heat_i <-
      renderPlot({
        raw_tevi_data() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = htr_i))
      })


    # display signal ------
    output$signal_plot <-
      renderPlot({
        raw_tevi_data() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = !!signal()))

      })

    # signal in range ------------------
    output$signal_in_selected_range <-
      renderPlot({
        data_selection() %>%
          ggplot(aes(x = time)) +
          geom_line(aes(y = !!signal()))
      })

    # spectrum ---------
    output$spectrum <-
      renderPlotly({
        est_spec_plot <-
          est_spec() %>%
          ggplot(aes(x = freq, y = spec)) +
          geom_line() +
          labs(x = "Frequency [Hz]",
               y = "log(periodogram)")

        est_spec_plot %>%
          ggplotly()

      })

    # numerical summary --------------
    output$selected_signal_info <-
      renderUI({
        data <-
          data_selection() %>%
          select(time,!!signal())

        time_range <- data %$% range(time)
        time <- mean(time_range)
        time_span <- time_range[2] - time_range[1]

        {
          h5(
            str_glue(
              "Window center-point: {round(time, 2)} s and window-width: {round(time_span, 2)} s"
            )
          )
        }
      })

    output$spectrum_info <-
      renderUI({
        max_freq <-
          est_spec() %>%
          filter(near(spec, max(spec), tol = 0.5)) %>%
          summarize(max_freq = mean(freq))
        h5(str_glue("Maximum Freq.: {round(max_freq$max_freq, 2)} Hz"))
      })
  }
