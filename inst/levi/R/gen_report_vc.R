gen_report_view <- function(id){
  ns <- NS(id)
  tagList(
    HTML("To Do: add UI to customize report: @Markus Wie sollte so ein Report aussehen?"),
      downloadButton(ns("gen_report"), "Generate report"),
      bsTooltip(ns("gen_report"), "choose an alloy in Set-up sample specs submenu to generate a test-report", "right", options = list(container = "body"))
  )
}

gen_report_ctrl <- function(input, output, session, alloy, selected_tab, tasks, notifications){

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

