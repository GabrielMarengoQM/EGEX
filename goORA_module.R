# Gene Ontology ORA Module using bslib
library(shiny)
library(bslib)
library(clusterProfiler)
library(org.Hs.eg.db)
library(DT)
library(shinycssloaders)

goORAUI <- function(id, saved_gene_lists) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("gene_list_sel"),
                   "Select Gene List(s) to analyze:",
                   choices = NULL,
                   selected = NULL,
                   multiple = FALSE),
    selectInput(ns("ontology"),
                "Ontology",
                choices = c("BP", "MF", "CC"),
                selected = "BP"),
    numericInput(ns("pvalueCutoff"),
                 "P-value cutoff",
                 value = 0.05, min = 0, max = 1, step = 0.01),
    numericInput(ns("qvalueCutoff"),
                 "Q-value cutoff",
                 value = 0.2, min = 0, max = 1, step = 0.01),
    fluidRow(column(3, actionButton(ns("run_go"), "Run GO Overrepresentation Analysis"))),
    br(),
    withSpinner(DT::dataTableOutput(ns("go_results"))),
    br(),
    withSpinner(plotOutput(ns("go_plot"))),
    fluidRow(column(3, downloadButton(ns("download_go"), "Download Results")))
  )
}

goORAServer <- function(id, con, saved_gene_lists) {
  moduleServer(id, function(input, output, session) {

    # Get the gene mapping table from the database
    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })

    observe({
      # Update the gene list select input based on saved_gene_lists$data names
      choices <- names(saved_gene_lists$data)
      updateSelectizeInput(session, "gene_list_sel", choices = choices, selected = choices[1])
    })

    # Run analysis when button is clicked
    results <- eventReactive(input$run_go, {
      print('RUNNING GO ANALYSIS')

      selectedLists <- input$gene_list_sel
      # Retrieve gene IDs from the selected saved gene list
      genes <- saved_gene_lists$data[[selectedLists]]$genes
      # Map GeneIDs to entrez IDs using gene_mapping()
      genes <- gene_mapping() %>%
        dplyr::filter(GeneID %in% genes) %>%
        dplyr::pull(entrez_id) %>%
        unique()
      if (length(genes) == 0) return(NULL)

      # Run enrichGO using the user-specified parameters
      enrich_res <- enrichGO(gene          = genes,
                             OrgDb         = org.Hs.eg.db,
                             ont           = input$ontology,
                             pvalueCutoff  = input$pvalueCutoff,
                             qvalueCutoff  = input$qvalueCutoff,
                             readable      = TRUE)
      enrich_res
    })

    # Render results as a DataTable with error messages if necessary
    output$go_results <- DT::renderDataTable({
      validate(
        need(!is.null(results()), "Error: Analysis did not return any results.")
      )
      df <- as.data.frame(results())
      validate(
        need(nrow(df) > 0, "Error: No significant GO terms found.")
      )
      df
    })

    # Render a barplot of the top enriched GO terms with error messages if necessary
    output$go_plot <- renderPlot({
      validate(
        need(!is.null(results()), "Error: Analysis did not return any results.")
      )
      df <- as.data.frame(results())
      validate(
        need(nrow(df) > 0, "Error: No significant GO terms found.")
      )
      dotplot(results(), showCategory = 10)
    })

    # Download handler for the results
    output$download_go <- downloadHandler(
      filename = function() {
        paste0("go_ORA_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        validate(
          need(!is.null(results()), "Error: No results to download.")
        )
        df <- as.data.frame(results())
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
