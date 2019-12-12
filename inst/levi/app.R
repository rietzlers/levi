## app.R ##

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")$value

# header ------------------------------------------------------------------
header <-
  dashboardHeader(
    title = "levi",
    dropdownMenu(type = "notifications"),
    dropdownMenu(type = "messages"),
    dropdownMenu(type = "tasks")
  )

# sidebar ----------------------------------------------------------------
sidebar <-
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Display Signal", tabName = "display_signal", icon = icon("bar-chart-o"))
  ))

# body ------------------------
body <-
  dashboardBody(tabItems(
    tabItem(tabName = "dashboard", dashboardUI("dashboard1")),
    tabItem(tabName = "display_signal", signalPlotUI("signal1"))
  ))

ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")


server <-
  function(input, output, session) {

    c(raw_tevi_data, frame_rate) %<-% callModule(dashboard, "dashboard1")

    callModule(signalPlot, "signal1", reactive(raw_tevi_data()), reactive(frame_rate()))

  }

shinyApp(ui, server)
