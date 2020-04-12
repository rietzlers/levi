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
    menuItem("Signal-Analysis", icon = icon("signal"),
             menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
             menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
             menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
             menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")),
    menuItem("Surface-Tension", tabName = "surface_tension", icon = icon("microscope")),
    menuItem("Simulate Data", tabName = "simulate_data_tab", icon = icon("microscope")),
    uiOutput("resample_UI")
  )),
body =
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", dashboard_UI("main_dashboard")),
      tabItem(tabName = "simulate_data_tab", simulate_data_view("simulate_data")),
      tabItem(tabName = "surface_tension", surface_tension_analysis_UI("st_analysis")),
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
    output$resample_UI <- renderUI(resample_UI())
    resample_UI <- reactiveVal()
  } # dynamic sidebar content

  # global data ------
  model <- reactive({
    if(model_choice() == "sim_data"){
      return(sim_data_model())
    }
    tevi_model()
  }) #toggle between simulated and tevi-data

  # dashboard --------------
  c(tevi_model, sample_specs) %<-% callModule(dashboard_ctrl, "main_dashboard")
  # signal-analysis ----------
  {
    callModule(seewave_ctrl, "spec_osc", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", model, selected_sidebar_tab)
  }
  # data-simulation -----
  c(sim_data_model, model_choice) %<-% callModule(simulate_data_ctrl, "simulate_data", resample_UI, selected_sidebar_tab)
  # surface-tension-analysis ----------
  callModule(surface_tension_analysis_ctrl, "st_analysis", model, sample_specs, tasks, notifications)


  }

shinyApp(ui, server, enableBookmarking = "server")
