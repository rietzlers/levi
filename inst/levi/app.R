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
    uiOutput("signal_selection_UI"),
    menuItem("Setup Data",tabName = "data_setup", icon = icon("dashboard"),
             menuSubItem("Import Signals from Tevi (.csv)", tabName = "importTeviData"),
             menuSubItem("Set up sample specs", tabName = "setup_sample_specs")),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("signal")),
    menuItem("seewave", tabName = "Visualization", icon = icon("chart-bar"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    uiOutput("sample_specs_info_UI")
  ))
body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
      tabItem(tabName = "setup_sample_specs", "to do: setup/load sample specs from .xls sheet"),
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
    signal_selection_UI <- reactiveVal()
    output$signal_selection_UI <- renderUI({signal_selection_UI()})

    sample_spec_info_UI <- reactiveVal()
    output$sample_specs_info_UI <- renderUI({sample_spec_info_UI()})

    sample_specs <- callModule(sample_specs_ctrl, "sample_specs", sample_spec_info_UI, selected_sidebar_tab)
    tevi_model <-  callModule(importTeviData, "tdi")
    signal_selections <- callModule(signalAnalysis, "sa", tevi_model, sample_specs, signal_selection_UI, selected_sidebar_tab)

    callModule(seewave_ctrl, "spec_osc", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", tevi_model, signal_selections, selected_sidebar_tab)
  }

shinyApp(ui, server)
