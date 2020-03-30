# library(reactlog)
# options(shiny.reactlog = TRUE)

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")

ui <- function(request){
  dashboardPage(title = "Alloy-EML-Analysis",
  header =
  dashboardHeader(
    title = "levi",
    dropdownMenuOutput("notifications"),
    dropdownMenuOutput("tasks"),
    dropdownMenuOutput("messages")
  ),
sidebar =
  dashboardSidebar(sidebarMenu(id = "sidebarMenu_ID",
    menuItem("Dashboard", icon = icon("dashboard"),
             bookmarkButton(label = "Save Session"),
             menuSubItem("Import Signals from Tevi (.csv)", tabName = "importTeviData", icon = icon("upload")),
             menuSubItem("Set up sample specs", tabName = "setup_sample_specs", icon = icon("database")),
             menuSubItem("Simulate Data", tabName = "simulate_data_tab", icon = icon("microscope")),
             menuSubItem("Report-notes", tabName = "gen_report", icon = icon("download"))),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("signal")),
    menuItem("seewave", icon = icon("chart-bar"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    uiOutput("resample_UI"),
    uiOutput("signal_selection_UI"),
    uiOutput("spectrum_view_UI"),
    uiOutput("spectrum_results_UI")
  )),
body =
  dashboardBody(
    tabItems(
      tabItem(tabName = "importTeviData", importTeviDataUI("tdi")),
      tabItem(tabName = "setup_sample_specs", sample_specs_view("sample_specs")),
      tabItem(tabName = "simulate_data_tab", simulate_data_view("simulate_data")),
      tabItem(tabName = "signalAnalysis", signalAnalysisUI("sa")),
      tabItem(tabName = "spec_osc", seewave_view("spec_osc")),
      tabItem(tabName = "spec_dom_freq", seewave_view("spec_dom_freq")),
      tabItem(tabName = "inst_freqs", seewave_view("inst_freqs")),
      tabItem(tabName = "sig_envelope", seewave_view("sig_envelope")),
      tabItem(tabName = "gen_report", gen_report_view("gen_report"))
      )
    )
)
}

server <- function(input, output, session) {
  # notes, tasks, msgs ------
  {
    output$notifications <- renderMenu({dropdownMenu(type = "notifications", .list = notifications())})
    output$tasks <- renderMenu({dropdownMenu(type = "tasks", .list = tasks())})
    output$messages <- renderMenu({dropdownMenu(type = "messages", .list = msgs())})

    notifications <- reactiveVal(list())
    tasks <- reactiveVal({})
    msgs <- reactiveVal(list())

    observeEvent(tevi_model(), {
        task_list <- tasks()
        task_list[["select_alloy"]] <-  taskItem(text = "Set sample-specs", value = 0, color = "red")
        tasks(task_list)
        showModal(modalDialog(
          title = "Uploaded Tevi-Data",
          "Make sure you set the sample-specifications to match the loaded tevi-data",
          easyClose = TRUE
        ))
      })# update tasks
    observeEvent(sample_specs(),{
      {
      c(alloy_name, d, m, Temp_liquid) %<-% sample_specs()
      notifications_list <- notifications()
      notifications_list[["alloy_info"]] <- notificationItem(text = str_glue("{alloy_name}, m = {m} g, d = {d} mm"),  status = "info", icon = icon("info-circle"))
      notifications(notifications_list)
      task_list <- tasks()
      task_list[["select_alloy"]] <-  NULL
      tasks(task_list)
      } # sample_specs
    })# update notes
  } # end notes, tasks and massages

  # dynamic sidebar content--------------
  {
    selected_sidebar_tab <- reactive(input$sidebarMenu_ID) #returns the selected sidebar-tab

    output$signal_selection_UI <- renderUI({signal_selection_UI()})
    signal_selection_UI <- reactiveVal()
    output$sample_specs_info_UI <- renderUI({sample_spec_info_UI()})
    sample_spec_info_UI <- reactiveVal()
    output$resample_UI <- renderUI(resample_UI())
    resample_UI <- reactiveVal()
    output$spectrum_view_UI <- renderUI(spectrum_view_UI())
    spectrum_view_UI <- reactiveVal()
    output$spectrum_results_UI <- renderUI(spectrum_results_UI())
    spectrum_results_UI <- reactiveVal()
  } # dynamic sidebar content

  # global data ------
  model <- reactive({
    if(model_choice() == "sim_data"){
      return(sim_data_model())
    }
    tevi_model()
  }) #toggle between simulated and tevi-data

  # module-calls --------------
  tevi_model <-  callModule(importTeviData, "tdi")
  sample_specs <- callModule(sample_specs_ctrl, "sample_specs")

  c(sim_data_model, model_choice) %<-% callModule(simulate_data_ctrl, "simulate_data", resample_UI, selected_sidebar_tab)

  signal_selections <- callModule(signalAnalysis, "sa",
                                  model, sample_specs, selected_sidebar_tab,
                                  signal_selection_UI,  signal_view_UI, spectrum_view_UI, spectrum_results_UI,
                                  tasks, notifications)

  callModule(gen_report_ctrl, "gen_report", sample_specs, selected_sidebar_tab, tasks, notifications)

  {
    callModule(seewave_ctrl, "spec_osc", model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", model, signal_selections, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", model, signal_selections, selected_sidebar_tab)
  } # seewave-plots
  }

shinyApp(ui, server, enableBookmarking = "server")
