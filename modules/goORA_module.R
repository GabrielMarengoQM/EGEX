library(clusterProfiler)
library(org.Hs.eg.db)
library(shiny)
library(bslib)
library(future)
library(ggplot2)   # For ggsave
plan(multisession)

## UI: Use a bslib collapsible sidebar layout (page_sidebar)
goORAUI <- function(id, saved_gene_lists) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = "Analysis Inputs",
      h5('Query list:'),
      selectizeInput(ns("gene_list_sel"),
                     "Select Gene List(s) to analyze:",
                     choices = NULL,
                     selected = NULL,
                     multiple = FALSE),
      hr(),
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
      actionButton(ns("recalculate"), "Run Analysis")
    ),
    uiOutput(ns("results_ui"))
  )
}

## Server: Creates a new task each time the button is clicked
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

    # Reactive list to store multiple ExtendedTask objects along with header info.
    # Each element is a list(task = <ExtendedTask>, header = <character>)
    tasks <- reactiveVal(list())

    # When the "Run Analysis" button is pressed, create a new task.
    observeEvent(input$recalculate, {
      # Capture the current reactive values
      localOntology <- isolate(input$ontology)
      localPvalueCutoff <- isolate(input$pvalueCutoff)
      localQvalueCutoff <- isolate(input$qvalueCutoff)

      # Retrieve gene IDs from the selected saved gene list
      genes <- saved_gene_lists$data[[input$gene_list_sel]]$genes
      # Map GeneIDs to entrez IDs using gene_mapping()
      genes <- gene_mapping() %>%
        dplyr::filter(GeneID %in% genes) %>%
        dplyr::pull(entrez_id) %>%
        unique()

      # Build a header string using the selected gene list and input parameters.
      header_info <- paste0("Gene List: ", input$gene_list_sel,
                            " | Ontology: ", localOntology,
                            " | P-value cutoff: ", localPvalueCutoff,
                            " | Q-value cutoff: ", localQvalueCutoff)

      # Create a new ExtendedTask instance that accepts ontology, p-value, and q-value
      new_task <- ExtendedTask$new(function(ont, pval, qval) {
        future({
          enrich_res <- enrichGO(
            gene          = genes,
            OrgDb         = org.Hs.eg.db,
            ont           = ont,
            pvalueCutoff  = pval,
            qvalueCutoff  = qval
          )
          enrich_res
        }, seed = TRUE)
      })

      # Invoke the task with the captured parameters
      new_task$invoke(localOntology, localPvalueCutoff, localQvalueCutoff)

      # Append the new task and its header info to the list of tasks
      current_tasks <- tasks()
      current_tasks[[length(current_tasks) + 1]] <- list(task = new_task, header = header_info)
      tasks(current_tasks)
    })

    # Dynamically generate a UI card (with a plot, table, and download buttons in the footer) for each task.
    output$results_ui <- renderUI({
      ns <- session$ns
      current_tasks <- tasks()

      fluidRow(
        lapply(seq_along(current_tasks), function(i) {
          # Retrieve header info for task i.
          header_info <- current_tasks[[i]]$header
          column(
            width = 6,
            card(
              full_screen = TRUE,
              height = '400px',
              card_header(
                h4(header_info)
              ),
              card_body(
                plotOutput(ns(paste0("result_task_", i))) %>% withSpinner(),
                br(),
                dataTableOutput(ns(paste0("table_task_", i)))
              ),
              card_footer(
                div(
                  style = "display: flex; gap: 10px;",
                  downloadButton(ns(paste0("download_plot_", i)), "Download Plot"),
                  downloadButton(ns(paste0("download_table_", i)), "Download Table")
                )
              )
            )
          )
        })
      )
    })

    # For each task in the list, create corresponding plot, table, and download handlers.
    observe({
      current_tasks <- tasks()
      lapply(seq_along(current_tasks), function(i) {
        task_obj <- current_tasks[[i]]$task
        local({
          my_index <- i
          plot_id <- paste0("result_task_", my_index)
          table_id <- paste0("table_task_", my_index)
          download_plot_id <- paste0("download_plot_", my_index)
          download_table_id <- paste0("download_table_", my_index)

          # Plot output
          output[[plot_id]] <- renderPlot({
            enrich_res <- task_obj$result()
            if (is.null(enrich_res)) {
              # If NULL, display "invalid input"
              plot.new()
              text(0.5, 0.5, "invalid input", cex = 1.5)
              return()
            }
            df <- as.data.frame(enrich_res)
            if(nrow(df) == 0) {
              # If no enriched terms found, display message
              plot.new()
              text(0.5, 0.5, "No enriched terms found", cex = 1.5)
              return()
            }
            dotplot(enrich_res, showCategory = 10)
          })

          # Table output
          output[[table_id]] <- renderDataTable({
            enrich_res <- task_obj$result()
            if (is.null(enrich_res)) return(data.frame(Message = "invalid input"))
            df <- as.data.frame(enrich_res)
            if(nrow(df) == 0) {
              return(data.frame(Message = "No enriched terms found"))
            }
            datatable(df)
          })

          # Download handler for plot
          output[[download_plot_id]] <- downloadHandler(
            filename = function() {
              paste0("plot_result_", my_index, ".png")
            },
            content = function(file) {
              enrich_res <- task_obj$result()
              if (is.null(enrich_res)) {
                png(file, width = 800, height = 600)
                plot.new()
                text(0.5, 0.5, "invalid input", cex = 1.5)
                dev.off()
                return()
              }
              df <- as.data.frame(enrich_res)
              if(nrow(df) == 0) {
                png(file, width = 800, height = 600)
                plot.new()
                text(0.5, 0.5, "No enriched terms found", cex = 1.5)
                dev.off()
              } else {
                p <- dotplot(enrich_res, showCategory = 10)
                ggsave(file, plot = p, device = "png", width = 8, height = 6)
              }
            }
          )

          # Download handler for table
          output[[download_table_id]] <- downloadHandler(
            filename = function() {
              paste0("table_result_", my_index, ".csv")
            },
            content = function(file) {
              enrich_res <- task_obj$result()
              if (is.null(enrich_res)) {
                write.csv(data.frame(Message = "invalid input"), file, row.names = FALSE)
                return()
              }
              df <- as.data.frame(enrich_res)
              if(nrow(df) == 0) {
                write.csv(data.frame(Message = "No enriched terms found"), file, row.names = FALSE)
              } else {
                write.csv(df, file, row.names = FALSE)
              }
            }
          )
        })
      })
    })

  })
}
