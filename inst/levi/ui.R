source(file.path("load_dependencies.R"),
       local = TRUE,
       encoding = "UTF-8")$value


# header ------------------------------------------------------------------

header <-
    dashboardHeader(
        title = "EML",
        dropdownMenu(type = "notifications"),
        dropdownMenu(type = "messages"),
        dropdownMenu(type = "tasks")
    )



# sidebar -----------------------------------------------------------------

sidebar <-
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Load tevi-data",
            tabName = "load_data",
            icon = icon("th"),
            badgeLabel = "first",
            badgeColor = "green"
        ),
        menuItem("Display Signal", tabName = "display_signal", icon = icon("bar-chart-o"))
        #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ))




body <-
    dashboardBody(tabItems(
        # load data ---------------------
        tabItem(
            tabName = "load_data",
                # upload file -
                shinydashboard::box(
                    title = "select tevi-data (.dat-file)",
                    width = 12,
                    fileInput(
                        "file",
                        label = NULL,
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv",
                                   ".dat")
                    )
                ),
                
                shinydashboard::box(
                    collapsible = TRUE,
                    width = 12,
                    title = "Set parameters",
                    div(
                        box(width = 4, numericInput("frame_rate", label = "Frame-Rate [Hz]", value = NULL)),
                        box(width = 4, numericInput("sample_mass", label = "Sample-Mass[g]", value = NULL)),
                        box(width = 4, numericInput("sphere_radius", label = "Sphere-Radius [mm]", value = NULL))
                    )
                ),
                
                # display data-table -
                shinydashboard::box(
                    collapsible = TRUE,
                    width = 12,
                    title = "Imported data",
                    DT::DTOutput("raw_tevi_data_table")
                    
                )
            ),
        
        # display signal --------------------
        tabItem(
            tabName = "display_signal",
            box(
                width = 12,
                title = "Signal Plot",
                selectInput("signal_choice", label = "Select Signal", choices = NULL),
                plotOutput(
                    "signal_plot",
                    brush = brushOpts(
                        id = "signal_plot_brush",
                        fill = "#ccc",
                        direction = "x"
                    ),
                    height = 250
                )
            ),
            box(
                title = "Signal in selected Range and corresponding spectrum",
                width = 12,
                box(width = 6,
                    plotOutput("signal_in_selected_range",
                               height = 250)
                    ),
                box(width = 6,
                    plotlyOutput("spectrum",
                                 height = 250)
                    )
            ),
            box(
                width = 6,
                uiOutput("selected_signal_info")
            ),
            box(
                width = 6,
                uiOutput("spectrum_info")
            )
        )
    ))


# setup page -----------------
dashboardPage(header, sidebar, body)

