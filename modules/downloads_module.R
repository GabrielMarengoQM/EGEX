##### ========================= Downloads Module UI ========================= #####
downloadsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Left column: download links for the entire database and each table.
      column(
        width = 6,
        h3("Download Data"),
        # Download link for the entire database packaged as ZIP.
        downloadLink(ns("download_all"), "Download Entire Database (ZIP)"),
        hr(),
        # Dynamically generated links for each table.
        uiOutput(ns("table_download_links"))
      ),
      # Right column: download link for the README file.
      column(
        width = 6,
        h3("Download README"),
        downloadLink(ns("download_readme"), "Download README")
      )
    )
  )
}

##### ========================= Downloads Module Server ========================= #####
downloadsServer <- function(id, con, saved_gene_lists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Retrieve list of tables from the DuckDB connection.
    tbls <- dbListTables(con)
    
    # Dynamically render download links (as links, not buttons) for each table.
    output$table_download_links <- renderUI({
      # For each table in tbls, create a downloadLink followed by a line break.
      table_links <- lapply(tbls, function(tbl) {
        tagList(
          downloadLink(ns(paste0("download_", tbl)), paste("Download Table:", tbl)),
          br()
        )
      })
      do.call(tagList, table_links)
    })
    
    # Download handler for the entire DuckDB packaged as a ZIP file.
    output$download_all <- downloadHandler(
      filename = function() {
        paste0("duckdb_data_", Sys.Date(), ".zip")
      },
      content = function(file) {
        # Create a temporary folder to hold CSV files and README.
        temp_folder <- tempfile(pattern = "duckdb_package")
        dir.create(temp_folder)
        
        # Write each table to a CSV file in the temporary folder.
        for (tbl in tbls) {
          out_file <- file.path(temp_folder, paste0(tbl, ".csv"))
          data <- dbReadTable(con, tbl)
          write.csv(data, out_file, row.names = FALSE)
        }
        
        # Create a README file with placeholder content and current timestamp.
        readme_file <- file.path(temp_folder, "README.txt")
        readme_text <- paste0("Data downloaded on: ", Sys.time(), "\n",
                              "Data Information and sources\n",
                              "Distribution License: [Placeholder]\n")
        writeLines(readme_text, con = readme_file)
        
        # Zip the entire temporary folder into one file.
        zip::zipr(zipfile = file, files = list.files(temp_folder, full.names = TRUE))
      }
    )
    
    # Create a download handler for each individual table.
    for (tbl in tbls) {
      local({
        my_table <- tbl  # Capture the current table name.
        output[[paste0("download_", my_table)]] <- downloadHandler(
          filename = function() {
            paste0(my_table, ".csv")
          },
          content = function(file) {
            data <- dbReadTable(con, my_table)
            write.csv(data, file, row.names = FALSE)
          }
        )
      })
    }
    
    # Download handler for the README file (standalone).
    output$download_readme <- downloadHandler(
      filename = function() {
        "README.txt"
      },
      content = function(file) {
        readme_text <- paste0("Data downloaded on: ", Sys.time(), "\n",
                              "Data Information and sources\n",
                              "Distribution License: [Placeholder]\n")
        writeLines(readme_text, con = file)
      }
    )
  })
}