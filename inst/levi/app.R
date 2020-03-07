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
  dashboardSidebar(sidebarMenu(id = "sidebarMenu",
    menuItem("Import Tevi Data",tabName = "importTeviData", icon = icon("dashboard")),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("bar-chart-o")),
    uiOutput("signal_selection")
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

    c(tevi_data, tevi_data_name, exp_time_range, frame_rate, mass, radius) %<-%
      callModule(importTeviData, "tdi")

    ss_input <- reactiveVal()
    output$signal_selection <- renderUI(ss_input())

    callModule(signalAnalysis, "sa", tevi_data, tevi_data_name, exp_time_range, frame_rate, mass, radius, ss_input, reactive(input$sidebarMenu))

  }

shinyApp(ui, server)
