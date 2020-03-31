report_notes_UI <- function(id){
  ns <- NS(id)
  tagList(
    textAreaInput(ns("notes"),
                  label = HTML("Report Notes (You can use <a href='https://www.markdownguide.org/cheat-sheet'>markdown</a> to format your notes.)"),
                  width = "100%", rows = 10,
                  placeholder = "Notes on analysis."),
    downloadButton(ns("gen_report"), "Download Notes as html-document"),
    bsTooltip(ns("gen_report"), "download report-notes as html-file", "right", options = list(container = "body"))
  )
}

report_notes_ctrl <- function(input, output, session, sample_specs){

  output$gen_report <- downloadHandler(
    filename = "report_notes.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_notes.Rmd")
      file.copy("./report_templates/report_notes_template.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        alloy = "Alloy-Analysis",
        notes = input$notes
        )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_format = "html_document",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

