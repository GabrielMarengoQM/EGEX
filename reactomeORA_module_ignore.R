library(shiny)
library(bslib)
library(clusterProfiler)
library(ReactomePA)
library(DT)
library(shinycssloaders)

# Module UI function for Reactome ORA
reactomeORAUI <- function(id, saved_gene_lists) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("gene_list_sel"),
                   "Select Gene List(s) to analyze:",
                   choices = NULL,
                   selected = NULL,
                   multiple = FALSE),
    numericInput(ns("pvalueCutoff"),
                 "P-value cutoff",
                 value = 0.05, min = 0, max = 1, step = 0.01),
    numericInput(ns("qvalueCutoff"),
                 "Q-value cutoff",
                 value = 0.2, min = 0, max = 1, step = 0.01),
    fluidRow(column(3, actionButton(ns("run_reactome"), "Run Reactome Overrepresentation Analysis"))),
    br(),
    withSpinner(DT::dataTableOutput(ns("reactome_results"))),
    br(),
    withSpinner(plotOutput(ns("reactome_plot"))),
    fluidRow(column(3, downloadButton(ns("download_go"), "Download Results")))
  )
}

# Module Server function for Reactome ORA
reactomeORAServer <- function(id, con, saved_gene_lists) {
  moduleServer(id, function(input, output, session) {

    # Retrieve gene mapping table from DB
    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })

    # Update gene list choices from saved_gene_lists
    observe({
      choices <- names(saved_gene_lists$data)
      updateSelectizeInput(session, "gene_list_sel", choices = choices, selected = choices[1])
    })

    # Run analysis when button is clicked
    results <- eventReactive(input$run_reactome, {
      print("RUNNING REACTOME ANALYSIS")

      selectedLists <- input$gene_list_sel
      # Retrieve gene IDs from the selected saved gene list
      genes <- saved_gene_lists$data[[selectedLists]]$genes

      # Map the GeneIDs to entrez IDs using gene_mapping
      genes <- gene_mapping() %>%
        dplyr::filter(GeneID %in% genes) %>%
        dplyr::pull(entrez_id) %>%
        unique()

      if (length(genes) == 0) return(NULL)

      # Run Reactome over-representation analysis using enrichPathway
      enrich_res <- enrichPathway(gene = genes,
                                  organism = "human",
                                  pvalueCutoff = input$pvalueCutoff,
                                  qvalueCutoff = input$qvalueCutoff,
                                  readable = TRUE)
      enrich_res
    })

    # Render results as a DataTable with validation
    output$reactome_results <- DT::renderDataTable({
      validate(
        need(!is.null(results()), "Error: Analysis did not return any results.")
      )
      df <- as.data.frame(results())
      validate(
        need(nrow(df) > 0, "Error: No significant Reactome pathways found.")
      )
      df
    })

    # Render a barplot of the top enriched Reactome pathways with validation
    output$reactome_plot <- renderPlot({
      validate(
        need(!is.null(results()), "Error: Analysis did not return any results.")
      )
      df <- as.data.frame(results())
      validate(
        need(nrow(df) > 0, "Error: No significant Reactome pathways found.")
      )
      # barplot(results(), showCategory = 10)
      dotplot(results(), showCategory = 10)
    })

    # Download handler for the results
    output$download_go <- downloadHandler(
      filename = function() {
        paste0("reactome_ORA_results_", Sys.Date(), ".csv")
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
