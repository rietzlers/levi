report_notes_UI <- function(id){
  ns <- NS(id)
  tagList(
    textAreaInput(ns("notes"),
                  label = HTML("Report Notes ( <a href='https://www.markdownguide.org/cheat-sheet'>Markdown Cheat-Sheet</a>)"), width = "100%", rows = 10,
                  placeholder = "Notes on analysis."),
    downloadButton(ns("gen_report"), "Download Notes as html-file"),
    bsTooltip(ns("gen_report"), "download report-notes as html-file", "right", options = list(container = "body"))
  )
}

report_notes_ctrl <- function(input, output, session, alloy, selected_tab, tasks, notifications){


  output$gen_report <- downloadHandler(

    # For PDF output, change this to "report.pdf"
    filename = "report_notes.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_notes.Rmd")
      file.copy("./report_templates/report_notes_template.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        notes = input$notes
        )

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

