## app.R ##

library(reactlog)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")

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
    menuItem("Signal Analysis", tabName = "signal_analysis", icon = icon("bar-chart-o"))
  ))


body <-
  dashboardBody(tabItems(
    tabItem(tabName = "dashboard", dashboardUI("dashboard1")),
    tabItem(tabName = "signal_analysis", signalAnalysisUI("analysis"))
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

    ctrls <-
      list(
        centerSignal = reactive(FALSE),
        smoothSignal = reactive(0)
      )

    # modules -------------

    c(model$tevi_data, model$frame_rate) %<-% callModule(dashboard, "dashboard1")

    callModule(signalAnalysis, "analysis", model$tevi_data, model$frame_rate)

  }

shinyApp(ui, server)
