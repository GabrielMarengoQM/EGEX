# Gene Ontology ORA
library(shiny)
library(bslib)
library(clusterProfiler)
library(org.Hs.eg.db)
library(DT)

goORAUI <- function(id, saved_gene_lists) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("gene_list_sel"),
                   "Select Gene List(s) to analyze:",
                   choices = NULL,
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

    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })

    observe({
      choices <- names(saved_gene_lists$data)
      updateSelectizeInput(session, "gene_list_sel", choices = choices, selected = choices)
    })

    # Run analysis when button is clicked
    results <- eventReactive(input$run_go, {
      print('RUNNING ANALYSIS')

      selectedLists <- input$gene_list_sel
      genes <- saved_gene_lists$data[[input$gene_list_sel]]$genes
      genes <- gene_mapping() %>%
        filter(GeneID %in% genes) %>%
        pull(entrez_id) %>%
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

    # Render results as a DataTable
    output$go_results <- DT::renderDataTable({
      req(results())
      as.data.frame(results())
    })

    # Render a barplot of the top enriched GO terms
    output$go_plot <- renderPlot({
      req(results())
      barplot(results(), showCategory = 10)
    })

    # Download handler for the results
    output$download_go <- downloadHandler(
      filename = function() {
        paste0("go_ORA_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(results())
        df <- as.data.frame(results())
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
