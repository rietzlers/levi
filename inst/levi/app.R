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
    sample_specs_view("sample_specs"),
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

    selectedSidebarMenu <- reactive(input$sidebarMenu) #returns the selected sidebar-tab
    dynamicSidebarItems <- reactiveValues(signal_selection = NULL)
    output$signal_selection <- renderUI({
      dynamicSidebarItems$signal_selection
      })

    sample_specs <- callModule(sample_specs_ctrl, "sample_specs")

    tevi_model <-  callModule(importTeviData, "tdi")

    callModule(signalAnalysis, "sa", tevi_model, sample_specs, dynamicSidebarItems, selectedSidebarMenu)

  }

shinyApp(ui, server)
