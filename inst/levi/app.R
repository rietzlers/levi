# library(reactlog)
# options(shiny.reactlog = TRUE)

source(file.path("R/load_dependencies.R"), local = TRUE, encoding = "UTF-8")

ui <- function(request) {
  dashboardPage(
    title = "Alloy-EML-Analysis",
    header =
      dashboardHeader(
        title = "levi",
        dropdownMenuOutput("notifications"),
        dropdownMenuOutput("tasks"),
        dropdownMenuOutput("messages")
      ),
    sidebar =
      dashboardSidebar(
        sidebarMenu(
          id = "sidebarMenu_ID",
          menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
          ),
          menuItem(
            "Signal-Analysis", icon = icon("signal"),
            menuSubItem("Spectrum and Oscillogram", tabName = "spec_osc"),
            menuSubItem("Spec+Dom-Freq", tabName = "spec_dom_freq"),
            menuSubItem("Instantanous Frequency", tabName = "inst_freqs"),
            menuSubItem("Smoothed Signal Envelope", tabName = "sig_envelope")
          ),
          menuItem(
            "Surface-Tension",icon = icon("tint"),
            menuSubItem("ST-Analysis", icon = icon("diagnoses"), tabName = "st_analysis"),
            menuSubItem("ST-Results", icon = icon("receipt"), tabName = "st_results")

          ),
          menuItem(
            "Viscosity",
            tabName = "viscosity_analysis_UI",
            icon = icon("vial")
          ),
          menuItem(
            "Simulate Data",
            tabName = "simulate_data_UI",
            icon = icon("blind")
          ),
          uiOutput("resample_UI"),
          bookmarkButton(
            label = "save session",
            title = HTML(
              "Paste the URL into your browsers location-bar and then bookmark it with your browser. (Make sure to give the bookmark a mneomic name.) You can send this URL to a collaborator to share your current analysis-results. (Your collaborator needs access to the same shiny-server!)"
            ),
            width = "80%"
          )
        )
      ),
    body =
      dashboardBody(
        tabItems(
          tabItem(tabName = "dashboard",
                  #tags$head(tags$script(src = "script.js")),
                  dashboard_UI("main_dashboard")),
          tabItem(tabName = "simulate_data_UI", simulate_data_view("simulate_data")),
          tabItem(tabName = "st_analysis", surface_tension_analysis_UI("st_analysis")),
          tabItem(tabName = "st_results", "plot and table"),
          tabItem(tabName = "viscosity_analysis_UI", "to be done"),
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
    observeEvent(tevi_model(), {
      notifications_list <- notifications()
      notifications_list[["tevi_data"]] <-
        notificationItem(
          text = tevi_model()$tevi_data_name,
          icon = icon("database")
          )
      notifications_list[["frame_rate"]] <-
        notificationItem(
          text = str_glue("Frame-Rate: {tevi_model()$frame_rate} Hz"),
          icon = icon("creative-commons-sampling")
          )
      notifications(notifications_list)
    })
    observeEvent(sample_specs(),{
      {
      c(alloy_name, d, m, Temp_liquid) %<-% sample_specs()
      notifications_list <- notifications()
      notifications_list[["alloy_info"]] <-
        notificationItem(text = str_glue("{alloy_name}, m = {m} g, d = {d} mm"),
                         status = "info",
                         icon = icon("fingerprint"))
      notifications(notifications_list)
      } # sample_specs
    })# update notes
  }

  # dynamic sidebar content--------------
  {
    selected_sidebar_tab <- reactive(input$sidebarMenu_ID) #returns the selected sidebar-tab
    output$resample_UI <- renderUI(resample_UI())
    resample_UI <- reactiveVal()
  }

  # global data ------
  model <- reactive({
    if(model_choice() == "sim_data") return(sim_data_model())
    tevi_model()
  }) #toggle between simulated and tevi-data

  # dashboard --------------
  c(tevi_model, sample_specs) %<-% callModule(dashboard_ctrl, "main_dashboard")

  # data-simulation -----
  c(sim_data_model, model_choice) %<-% callModule(simulate_data_ctrl, "simulate_data", resample_UI)
  # surface-tension-analysis ----------
  callModule(surface_tension_analysis_ctrl, "st_analysis", model, sample_specs, tasks, notifications)

  # signal-analysis ----------
  {
    callModule(seewave_ctrl, "spec_osc", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "spec_dom_freq", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "inst_freqs", model, selected_sidebar_tab)
    callModule(seewave_ctrl, "sig_envelope", model, selected_sidebar_tab)
  }

  # stop app when browser-window is closed by user
  # session$onSessionEnded(stopApp)
  }

shinyApp(ui, server, enableBookmarking = "server")
