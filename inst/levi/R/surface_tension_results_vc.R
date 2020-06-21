
surface_tension_results_UI <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        fluidRow(
          column(width = 1,
                 selectInput(ns("selected_view"), label = "show", choices = c("ST-Plot" = "st_plot", "ST-Datatable" = "st_datatable")),
                 actionButton(ns("show_ctrls"), label = NULL, icon = icon("wrench"),  width = "100%"),
                 bsTooltip(ns("show_ctrls"), "Show additional controls")
          ),
          column(width = 11,
                 tags$style(paste("#", ns("tab_wizard"), " { display:none; }", sep = "")),
                 tabsetPanel(id = ns("tab_wizard"),
                             tabPanel(ns("st_plot"),
                                      plotlyOutput(ns("surface_tension_plot"), height = "400px")
                                      ),
                             tabPanel(ns("st_datatable"),
                                      surface_tension_result_data_UI(ns("st_result_data"))
                                      )
                            )
                 )
          )
        ),
    bsModal(ns("additional_ctrls"), title = "Additional Controls for Surface-Tension-Results-Data", trigger = ns("show_ctrls"),
            box(title = "Axis-Scaling of result-plot",
                fluidRow(
                  column(width = 6,
                         selectInput(ns("st_yvar"), label = NULL, selected = "st", choices = c("Surface-Tension [Nm]" = "st", "Freq. [Hz]" = "f"))),
                  column(width = 6,
                         selectInput(ns("st_xvar"), label = NULL, selected = "temp", choices = c("Temp. [° C]" = "temp", "time [s]" = "t"))),
                  tags$div("Time = 0s is different for each tevi-data-set! This means you have to be carful in interpreting the plot.
                           Only temperature allows unambigous interpretation.")
                )
            ),
            size = "large")
    )
}

surface_tension_results_ctrl <- function(input, output, session, tevi_model, sample_specs, live_parameter_estimates, add_result){

  observe(
    updateTabsetPanel(session, "tab_wizard", selected = session$ns(input$selected_view))
  )

  # st-result-plot  -----------
  output$surface_tension_plot <- renderPlotly({

    validate(need(spec_analysis_results(), message = "need spec-analysis-results"))

      spec_analysis_results() %>%
        plot_ly(source = "st_plot") %>%
        add_markers(x = ~ t, y = ~ f_0, name = "f_0", hovertemplate = "%{y:.1f}  Hz",
                    marker = list(color = "red")) %>%
        add_markers(x = ~ t, y = ~ f_dom, name = "f_dom", hovertemplate = "%{y:.1f}  Hz",
                    marker = list(color = "black")) %>%
        add_markers(data = live_parameter_estimates(), name = "f_0 (current)",
                    x = ~ t, y = ~ f_0, name = "f_0", hovertemplate = "%{y:.1f}  Hz",
                    marker = list(color = "red", symbol = c("x"), size = 10, opacity = 0.5),
                    showlegend = FALSE) %>%
        add_markers(data = live_parameter_estimates(), name = "f_dom (current)",
                    x = ~ t, y = ~ f_dom, name = "f_dom", hovertemplate = "%{y:.1f}  Hz",
                    marker = list(color = "black", symbol = c("x"), size = 10, opacity = 0.5),
                    showlegend = FALSE) %>%
        layout(
          legend = list(x = 0.8, y = 0.9),
          xaxis = list(title = "time [s]"),
          yaxis = list(title = "Freq [Hz]")
        )

    })

  spec_analysis_results <-
    callModule(surface_tension_results_data_ctrl, "st_result_data", tevi_model, sample_specs, live_parameter_estimates, add_result)

  # return-values ----------

}

