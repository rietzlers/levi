## app.R ##

library(reactlog)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")$value

# view --------
header <-
  dashboardHeader(
    title = "levi",
    dropdownMenu(type = "notifications"),
    dropdownMenu(type = "messages"),
    dropdownMenu(type = "tasks")
  )


sidebar <-
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Display Signal", tabName = "display_signal", icon = icon("bar-chart-o")),
    menuItem("Compare Signals", tabName = "compare_signals", icon = icon("microscope"))
  ))


body <-
  dashboardBody(tabItems(
    tabItem(tabName = "dashboard", dashboardUI("dashboard1")),
    tabItem(tabName = "display_signal", signalPlotUI("signal1")),
    tabItem(tabName = "compare_signals", compareSignalsUI("comp1"))
  ))

ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")

# controller -----------------
server <-
  function(input, output, session) {

    # model-specification -------------
    model <-
      list(
        tevi_data = tibble(),
        frame_rate = NA_real_,
        sample_mass = NA_real_,
        sample_radius = NA_real_,
        camara = NA_character_,
        alloy = NA_character_,
        exp_timing = tibble()
        )

    model <- map(model, ~ reactive(.x))

    # modules -------------

    c(model$tevi_data, model$frame_rate) %<-% callModule(dashboard, "dashboard1")

    callModule(signalPlot, "signal1", model$tevi_data, model$frame_rate)

    #callModule(compareSignals, "comp1", reactive(model$tevi_data), reactive(timing_info$frame_rate))
  }

shinyApp(ui, server)
