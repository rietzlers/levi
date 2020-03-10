library(reactlog)
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
  dashboardSidebar(sidebarMenu(id = "sidebarMenu_ID",
    menuItem("Setup Data",tabName = "data_setup", icon = icon("dashboard"),
             menuSubItem("Import Signals from Tevi (.csv)", tabName = "importTeviData"),
             menuSubItem("Set up sample specs", tabName = "setup_sample_specs")),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("signal")),
    uiOutput("signal_selection"),
    menuItem("seewave", tabName = "Visualization", icon = icon("chart-bar"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    sample_specs_view("sample_specs")
  ))
body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
      tabItem(tabName = "setup_sample_specs", "setup sample specs"),
      tabItem(tabName = "signalAnalysis", signalAnalysisUI("sa")),
      tabItem(tabName = "spec_osc", seewave_view("spec_osc")),
      tabItem(tabName = "spec_dom_freq", seewave_view("spec_dom_freq")),
      tabItem(tabName = "inst_freqs", seewave_view("inst_freqs")),
      tabItem(tabName = "sig_envelope", seewave_view("sig_envelope"))
      )
    )
ui <- dashboardPage(header, sidebar, body, title = "Alloy-EML-Analysis")
# ctrl --------
server <- function(input, output, session) {

    selected_sidebar_tab <- reactive(input$sidebarMenu_ID) #returns the selected sidebar-tab
    dynamicSidebarItems <- reactiveValues(signal_selection = NULL)
    output$signal_selection <- renderUI({
      dynamicSidebarItems$signal_selection
      })

    sample_specs <- callModule(sample_specs_ctrl, "sample_specs")
    tevi_model <-  callModule(importTeviData, "tdi")
    signal_selections <- callModule(signalAnalysis, "sa", tevi_model, sample_specs, dynamicSidebarItems, selected_sidebar_tab)

    callModule(seewave_ctrl, "spec_osc", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", tevi_model, signal_selections, selected_sidebar_tab)
  }

shinyApp(ui, server)
