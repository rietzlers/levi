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
    menuItem("Import Tevi Data", tabName = "importTeviData", icon = icon("dashboard")),
    menuItem("Signal Analysis", tabName = "signal_analysis", icon = icon("bar-chart-o"))
  ))


body <-
  dashboardBody(tabItems(
    tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
    tabItem(tabName = "signal_analysis", signalAnalysisUI("analysis"))
  ))

ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")

# controller -----------------
server <-
  function(input, output, session) {

    c(signals, frame_rate) %<-%
      callModule(importTeviData, "tdi")

    callModule(signalAnalysis, "analysis", signals, frame_rate)

  }

shinyApp(ui, server)
