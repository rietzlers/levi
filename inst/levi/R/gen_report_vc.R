gen_report_view <- function(id){
  ns <- NS(id)
}


gen_report_ctrl <- function(input, output, session, alloy, gen_report_UI, selected_tab, tasks, notifications){

  observeEvent(alloy(),{
    gen_report_UI(
      div(
        downloadButton(session$ns("gen_report"), "Generate report"),
        bsTooltip(session$ns("gen_report"), "if file is not found: maybe some parameters are not set", "right", options = list(container = "body")))
      )
  })

  output$gen_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "test_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "test_report.Rmd")
      file.copy("./report_templates/test_report_template.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(alloy_specs = alloy())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

