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
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Signal Analysis", tabName = "signalAnalysis", icon = icon("signal")),
    menuItem("Simulate Data", tabName = "simulate_data_tab", icon = icon("microscope")),
    menuItem("seewave", icon = icon("chart-bar"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    uiOutput("resample_UI"),
    uiOutput("spectrum_results_UI")
  )),
body =
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        bookmarkButton(label = "Save current results",
                       title=
                       HTML("Paste the URL into your browsers location-bar and then bookmark it with your browser. (Make sure to give the bookmark a mneomic name.) You can send this URL to a collaborator to share your current analysis-results. (Your collaborator needs access to the same shiny-server!)"),
                       width = "100%"),
        uiOutput("sample_and_exp_info"),
        tabsetPanel(
          tabPanel("Report-Notes", report_notes_UI("report_notes"), icon = icon("clipboard")),
          tabPanel("Experiment- and Alloy-Specification", icon = icon("wpexplorer"),
                   sample_specs_view("sample_specs")
                   ),
          tabPanel("Upload .csv-data from Tevi", load_tevi_data_UI("load_tevi_data"), icon = icon("upload"))
        )),
      tabItem(tabName = "simulate_data_tab", simulate_data_view("simulate_data")),
      tabItem(tabName = "signalAnalysis", signalAnalysisUI("sa")),
      tabItem(tabName = "spec_osc", seewave_view("spec_osc")),
      tabItem(tabName = "spec_dom_freq", seewave_view("spec_dom_freq")),
      tabItem(tabName = "inst_freqs", seewave_view("inst_freqs")),
      tabItem(tabName = "sig_envelope", seewave_view("sig_envelope"))
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

    observeEvent(tevi_model(), {})# update tasks
    observeEvent(sample_specs(),{
      {
      c(alloy_name, d, m, Temp_liquid) %<-% sample_specs()
      notifications_list <- notifications()
      notifications_list[["alloy_info"]] <- notificationItem(text = str_glue("{alloy_name}, m = {m} g, d = {d} mm"),  status = "info", icon = icon("info-circle"))
      notifications(notifications_list)
      } # sample_specs
    })# update notes
  } # end notes, tasks and massages

  # dynamic sidebar content--------------
  {
    selected_sidebar_tab <- reactive(input$sidebarMenu_ID) #returns the selected sidebar-tab

    output$signal_selection_UI <- renderUI({signal_selection_UI()})
    signal_selection_UI <- reactiveVal()
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

  # render-output ------
  output$sample_and_exp_info <- renderUI({
    c(alloy_name, m, radius, Temp_liquid) %<-% sample_specs()
    box(width = 12,
    HTML(
      str_glue("<b>Selected Alloy:</b> {alloy_name}
               </br><b>Sample-Specifications:</b> Mass of sample: {m} g; Diameter of Sample: {radius*2} mm; Liquidus-Temp: {Temp_liquid} K,
               </br> <b>Frame-Rate of camera:</b> {tevi_model()$frame_rate} Hz
               </br> <b>Tevi-Data:</b> {tevi_model()$tevi_data_name}")
    ))
    })
  # module-calls --------------
  tevi_model <-  callModule(load_tevi_data_ctrl, "load_tevi_data")
  sample_specs <- callModule(sample_specs_ctrl, "sample_specs")

  c(sim_data_model, model_choice) %<-% callModule(simulate_data_ctrl, "simulate_data", resample_UI, selected_sidebar_tab)

  analysis_parameters <- callModule(signalAnalysis, "sa",
                                  model, sample_specs, selected_sidebar_tab,
                                  signal_selection_UI,  signal_view_UI, spectrum_view_UI, spectrum_results_UI,
                                  tasks, notifications)

  callModule(report_notes_ctrl, "report_notes", sample_specs)

  {
    callModule(seewave_ctrl, "spec_osc", model, analysis_parameters, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", model, analysis_parameters, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", model, analysis_parameters, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", model, analysis_parameters, selected_sidebar_tab)
  } # seewave-plots
  }

shinyApp(ui, server, enableBookmarking = "server")
