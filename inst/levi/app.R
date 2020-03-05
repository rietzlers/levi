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
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("bar-chart-o"))
  ))


body <-
  dashboardBody(tabItems(
    tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
    tabItem(tabName = "signalAnalysis", signalAnalysisUI("sa"))
  ))

ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")

# controller -----------------
server <-
  function(input, output, session) {

    c(tevi_data, frame_rate, mass, radius) %<-%
      callModule(importTeviData, "tdi")

    callModule(signalAnalysis, "sa", tevi_data, frame_rate, mass, radius)

  }

shinyApp(ui, server)
