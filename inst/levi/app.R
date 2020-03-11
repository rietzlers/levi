library(reactlog)
options(shiny.reactlog = TRUE)

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")
# view --------
header <-
  dashboardHeader(
    title = "levi",
    dropdownMenuOutput("notifications"),
    dropdownMenuOutput("tasks"),
    dropdownMenuOutput("messages")
  )
sidebar <-
  dashboardSidebar(sidebarMenu(id = "sidebarMenu_ID",
    menuItem("Setup Data",tabName = "data_setup", icon = icon("dashboard"),
             menuSubItem("Import Signals from Tevi (.csv)", tabName = "importTeviData"),
             menuSubItem("Set up sample specs", tabName = "setup_sample_specs"),
             menuSubItem("Simulate Data", tabName = "simulate_data_tab")),
    uiOutput("signal_selection_UI"),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("signal")),
    menuItem("seewave", tabName = "Visualization", icon = icon("chart-bar"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    uiOutput("sample_specs_info_UI"),
    uiOutput("gen_report_UI")
  ))
body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
      tabItem(tabName = "setup_sample_specs", sample_specs_view("sample_specs")),
      tabItem(tabName = "simulate_data_tab", "to do: generate simulated data to analyse"),
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
  {
    output$notifications <- renderMenu({dropdownMenu(type = "notifications", .list = notifications())})
    output$tasks <- renderMenu({dropdownMenu(type = "tasks", .list = tasks())})
    output$messages <- renderMenu({dropdownMenu(type = "messages", .list = msgs())})

    notifications <- reactiveVal(list())
    tasks <- reactiveVal({
      list(select_alloy = taskItem(text = "Select alloy in sub-menu: 'Set up sample specs'", value = 0, color = "red"))
      })
    msgs <- reactiveVal(list())
  } # notes, tasks and massages
  {
    selected_sidebar_tab <- reactive(input$sidebarMenu_ID) #returns the selected sidebar-tab
    output$signal_selection_UI <- renderUI({signal_selection_UI()})
    signal_selection_UI <- reactiveVal()
    output$sample_specs_info_UI <- renderUI({sample_spec_info_UI()})
    sample_spec_info_UI <- reactiveVal()
    output$gen_report_UI <- renderUI({gen_report_UI()})
    gen_report_UI <- reactiveVal()
  } # dynamic sidebar content

  sample_specs <- callModule(sample_specs_ctrl, "sample_specs", sample_spec_info_UI, selected_sidebar_tab, tasks, notifications)
  tevi_model <-  callModule(importTeviData, "tdi")
  signal_selections <- callModule(signalAnalysis, "sa", tevi_model, sample_specs, signal_selection_UI, selected_sidebar_tab)
  callModule(gen_report_ctrl, "gen_report", sample_specs, gen_report_UI, selected_sidebar_tab, tasks, notifications)
  {
    callModule(seewave_ctrl, "spec_osc", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", tevi_model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", tevi_model, signal_selections, selected_sidebar_tab)
  } # seewave-plots
  }

shinyApp(ui, server)
