library(shiny)
library(DBI)
library(duckdb)
library(dplyr)
library(epitools)
library(ggplot2)
library(future)
library(DT)
library(purrr)
library(scales)  # for hue_pal()
plan(multisession)

### UI Module for Odds Ratio Analysis
oddsRatioUI <- function(id, saved_gene_lists) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = "Analysis Inputs",
        # 1. Choose the DuckDB table to filter on
      h5('Query list(s):'),
      selectizeInput(ns("gene_list_sel"),
                     "Select Gene List(s) to analyze:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE),
      hr(),
      h5('Genes to test against:'),
      selectInput(ns("selected_table"), "Select Table:", choices = NULL),
      # 2. Choose a column from that table
      selectInput(ns("selected_column"), "Select Column:", choices = NULL),
      # 3. Render filter widget dynamically (slider for numeric; selectize for text)
      uiOutput(ns("filter_ui")),
      hr(),
      # 4. Select one or more gene lists from your saved gene lists
      # Run Analysis – all inputs are required.
      actionButton(ns("recalculate"), "Run Analysis")
    ),
    fluidRow(uiOutput(ns("results_ui")))
  )
}

### Server Module for Odds Ratio Analysis
oddsRatioServer <- function(id, con, saved_gene_lists) {
  moduleServer(id, function(input, output, session) {

    ## Forest Plot Function
    # Expects a data frame with columns: Index, OR, LL, UL, padj, label, and Color.
    forestPlot <- function(data, plot_title) {
      p <- ggplot2::ggplot(data, ggplot2::aes(y = Index, x = as.numeric(OR), color = Color)) +
        ggplot2::geom_point(shape = 16, size = 4) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = as.numeric(LL), xmax = as.numeric(UL)),
                                height = 0.3, width = 0.2, linewidth = 1.2) +
        ggplot2::geom_vline(xintercept = 1, color = "black", linetype = "dashed", alpha = 0.5) +
        ggplot2::scale_y_continuous(breaks = 1:length(data$label),
                                    labels = data$label,
                                    trans = "reverse") +
        ggplot2::expand_limits(y = c(0.5, length(data$label) + 0.5)) +
        ggplot2::xlab("Odds ratio (95% CI)") +
        ggplot2::ylab(NULL) +
        ggplot2::ggtitle(plot_title) +
        ggplot2::geom_label(ggplot2::aes(label = paste0("OR ", OR, ", p = ", padj), color = Color),
                            fill = "white", vjust = -1.5, size = 3, label.size = 0.5) +
        # Use identity scale so that the colors we provide appear exactly as is.
        ggplot2::scale_color_identity() +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       axis.text.y = ggplot2::element_text(size = 12, colour = "black"),
                       axis.text.x.bottom = ggplot2::element_text(size = 12, colour = "black"),
                       plot.title = ggplot2::element_text(size = 12, face = "plain", hjust = 0.5),
                       legend.position = "none",
                       plot.margin = ggplot2::margin(t = 10, b = 10, l = 10, r = 10))
      return(p)
    }

    ## Gene Mapping: Read the "genes" table from DuckDB as the background set.
    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })

    ### 1. Update the table choices from DuckDB.
    observe({
      tbls <- dbListTables(con)
      updateSelectInput(session, "selected_table", choices = tbls)
    })

    ### 2. Update the saved gene list choices.
    observe({
      choices <- names(saved_gene_lists$data)
      updateSelectizeInput(session, "gene_list_sel", choices = choices,
                           selected = if (length(choices)) choices[1] else NULL)
    })

    ### 3. When a table is selected, update the column choices and clear old filters.
    observeEvent(input$selected_table, {
      req(input$selected_table)
      cols <- dbListFields(con, input$selected_table)
      updateSelectInput(session, "selected_column", choices = cols)
      output$filter_ui <- renderUI({ NULL })
    })

    ### 4. When the table or column changes, update the filtering widget.
    observeEvent({
      input$selected_table
      input$selected_column
    }, {
      req(input$selected_table, input$selected_column)
      sample_val <- tryCatch({
        dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1",
                                input$selected_column, input$selected_table))[[1]]
      }, error = function(e) NULL)

      if (is.null(sample_val)) {
        output$filter_ui <- renderUI({ NULL })
        return(NULL)
      }

      if (is.numeric(sample_val)) {
        # For numeric columns, get min and max and render a slider.
        query_range <- sprintf("SELECT MIN(%s) AS min_val, MAX(%s) AS max_val FROM %s",
                               input$selected_column, input$selected_column, input$selected_table)
        range_df <- dbGetQuery(con, query_range)
        output$filter_ui <- renderUI({
          sliderInput(session$ns("selected_value"),
                      "Select Range:",
                      min = range_df$min_val,
                      max = range_df$max_val,
                      value = c(range_df$min_val, range_df$max_val))
        })
      } else {
        # For non-numeric columns, get the distinct values.
        query <- sprintf("SELECT DISTINCT %s FROM %s WHERE %s IS NOT NULL",
                         input$selected_column,
                         input$selected_table,
                         input$selected_column)
        values <- dbGetQuery(con, query)[[1]]
        output$filter_ui <- renderUI({
          selectizeInput(session$ns("selected_value"),
                         "Select Value:",
                         choices = values,
                         multiple = FALSE)
        })
      }
    })

    ### 5. RUN ASYNC ANALYSIS when "Run Analysis" is clicked.
    # We do not use a separate "Get GeneIDs" button.
    tasks <- reactiveVal(list())

    observeEvent(input$recalculate, {
      # Make sure all required inputs are provided.
      req(input$selected_table, input$selected_column, input$selected_value, input$gene_list_sel)

      # Build the compare list using the current filter input.
      sample_val <- tryCatch({
        dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1",
                                input$selected_column, input$selected_table))[[1]]
      }, error = function(e) NULL)

      if (is.numeric(sample_val)) {
        query <- sprintf("SELECT GeneID FROM %s WHERE %s BETWEEN %s AND %s",
                         input$selected_table,
                         input$selected_column,
                         input$selected_value[1],
                         input$selected_value[2])
      } else {
        query <- sprintf("SELECT GeneID FROM %s WHERE %s = '%s'",
                         input$selected_table,
                         input$selected_column,
                         input$selected_value)
      }
      compare_list <- dbGetQuery(con, query)$GeneID

      # Retrieve the selected gene lists from saved_gene_lists.
      selected_list_names <- isolate(input$gene_list_sel)
      gene_list <- unique(unlist(lapply(selected_list_names, function(n) {
        saved_gene_lists$data[[n]]$genes
      })))

      # Background: all GeneIDs from the genes table.
      background <- gene_mapping() %>% dplyr::select(GeneID)

      # Build header string showing the table, column, and filter value.
      header_info <- paste0("Table: ", input$selected_table,
                            " | Column: ", input$selected_column,
                            " | Filter: ",
                            if (is.numeric(sample_val)) {
                              paste0("from ", input$selected_value[1], " to ", input$selected_value[2])
                            } else {
                              input$selected_value
                            })

      # For each selected gene list, calculate the odds ratio.
      results_list <- lapply(selected_list_names, function(name) {
        gl <- saved_gene_lists$data[[name]]$genes
        df_temp <- background %>%
          mutate(selected = ifelse(GeneID %in% gl, "y", "n"),
                 compared = ifelse(GeneID %in% compare_list, "y", "n"))
        ct <- table(df_temp$compared, df_temp$selected)
        odds <- epitools::oddsratio(ct, method = "wald")
        or_val <- round(odds$measure[2], 3)
        lower_val <- round(odds$measure[4], 3)
        upper_val <- round(odds$measure[6], 3)
        p_val <- odds$p.value[4]
        p_val_fmt <- formatC(p_val, format = "e", digits = 2)
        data.frame(label = name,
                   OR = or_val,
                   LL = lower_val,
                   UL = upper_val,
                   pvalue = p_val_fmt,
                   stringsAsFactors = FALSE)
      })
      results_df <- bind_rows(results_list)
      # Adjust p-values (padj) using the numeric version then format in scientific notation.
      numeric_pvals <- sapply(results_list, function(x) as.numeric(x$pvalue))
      padj <- p.adjust(numeric_pvals, method = "BH")
      results_df$padj <- formatC(padj, format = "e", digits = 2)

      # Generate distinct colors for each selected gene list.
      colors <- hue_pal()(length(selected_list_names))
      results_df$Color <- colors

      # Add row numbers (for plotting, used as Index).
      results_df <- results_df %>% mutate(Index = row_number())

      print(results_df)

      # Create an ExtendedTask instance that returns results_df asynchronously.
      new_task <- ExtendedTask$new(function() {
        future({
          results_df
        }, seed = TRUE)
      })
      new_task$invoke()

      # Append the task, header info, and gene list details to our tasks list.
      current_tasks <- tasks()
      current_tasks[[length(current_tasks) + 1]] <- list(
        task = new_task,
        header = header_info,
        gene_lists = selected_list_names,
        gene_list_values = gene_list,
        compare_list = compare_list
      )
      tasks(current_tasks)
    })

    ### 6. Generate the Analysis UI – one card per analysis task.
    output$results_ui <- renderUI({
      ns <- session$ns
      current_tasks <- tasks()
      fluidRow(
        lapply(seq_along(current_tasks), function(i) {
          header_info <- current_tasks[[i]]$header
          column(
            width = 6,
            card(
              full_screen = TRUE,
              height = '600px',  # Adjust the card height as needed
              card_header(
                h4(header_info)
              ),
              card_body(
                # Upper half: Forest plot; lower half: results table.
                plotOutput(ns(paste0("result_task_", i))) %>% withSpinner(),
                br(),
                dataTableOutput(ns(paste0("table_task_", i)))
              ),
              card_footer(
                div(
                  style = "display: flex; gap: 10px;",
                  downloadButton(ns(paste0("download_plot_", i)), "Download Plot"),
                  downloadButton(ns(paste0("download_table_", i)), "Download Table"),
                  downloadButton(ns(paste0("download_genelists_", i)), "Download Gene Lists")
                )
              )
            )
          )
        })
      )
    })

    ### 7. Create Plot, Table, and Download Handlers for Each Task.
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
          download_genelists_id <- paste0("download_genelists_", my_index)

          # Plot output: Render the forest plot.
          output[[plot_id]] <- renderPlot({
            enrich_res <- task_obj$result()
            if (is.null(enrich_res)) {
              plot.new()
              text(0.5, 0.5, "invalid input", cex = 1.5)
              return()
            }
            df <- as.data.frame(enrich_res)
            if (nrow(df) == 0) {
              plot.new()
              text(0.5, 0.5, "No enriched terms found", cex = 1.5)
              return()
            }
            forestPlot(df, "Odds Ratio Forest Plot")
          })

          # Table output: Render the odds ratio results as a data table.
          output[[table_id]] <- renderDataTable({
            enrich_res <- task_obj$result()
            if (is.null(enrich_res))
              return(data.frame(Message = "invalid input"))
            df <- as.data.frame(enrich_res)
            if (nrow(df) == 0)
              return(data.frame(Message = "No enriched terms found"))
            datatable(df)
          })

          # Download handler for forest plot as PNG.
          output[[download_plot_id]] <- downloadHandler(
            filename = function() {
              paste0("forest_plot_result_", my_index, ".png")
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
              if (nrow(df) == 0) {
                png(file, width = 800, height = 600)
                plot.new()
                text(0.5, 0.5, "No enriched terms found", cex = 1.5)
                dev.off()
              } else {
                p <- forestPlot(df, "Odds Ratio Forest Plot")
                ggsave(file, plot = p, device = "png", width = 8, height = 6)
              }
            }
          )

          # Download handler for the results table.
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
              if (nrow(df) == 0) {
                write.csv(data.frame(Message = "No enriched terms found"), file, row.names = FALSE)
              } else {
                write.csv(df, file, row.names = FALSE)
              }
            }
          )

          # Download handler for gene lists: Combine the saved gene list(s) and compare list.
          output[[download_genelists_id]] <- downloadHandler(
            filename = function() {
              paste0("gene_lists_", my_index, ".csv")
            },
            content = function(file) {
              gl <- current_tasks[[my_index]]$gene_list_values
              cl <- current_tasks[[my_index]]$compare_list
              n_max <- max(length(gl), length(cl))
              df_gene_lists <- data.frame(
                gene_list = c(gl, rep(NA, n_max - length(gl))),
                compare_list = c(cl, rep(NA, n_max - length(cl)))
              )
              write.csv(df_gene_lists, file, row.names = FALSE)
            }
          )
        })
      })
    })
  })
}
