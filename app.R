library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(reactable)
library(plotly)
library(dplyr)

# Connect to DuckDB
con <- dbConnect(duckdb(), "mydb.duckdb")
# Get all table names; exclude "aggregated" which is used for the aggregated data.
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "aggregated")

# Reactive value to store saved gene lists.
# Each saved gene list is a list with two elements: "genes" and "filters".
saved_gene_lists <- reactiveValues(data = list())

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Dynamic DuckDB Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Accordion with three panels:
      accordion(
        accordion_panel("Saved Gene Lists", uiOutput("saved_gene_lists_ui")),
        accordion_panel("Current Filters", uiOutput("current_filters")),
        accordion_panel("Filter Controls",
                        lapply(individual_tables, function(tbl) {
                          tagList(
                            h4(tbl),
                            uiOutput(paste0("filters_", tbl))
                          )
                        }))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 actionButton("save_gene_list", "Save Gene List"),
                 br(), br(),
                 reactableOutput("aggregated_table")
        ),
        tabPanel("Plot",
                 tabsetPanel(
                   tabPanel("Bar Chart",
                            selectInput("bar_table", "Select table for Bar Chart:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("bar_col", "Select column:",
                                        choices = NULL, selected = ""),
                            radioButtons("bar_y_type", "Y-axis type:",
                                         choices = c("Count", "Percentage"),
                                         selected = "Count", inline = TRUE),
                            # Multi-select for gene lists:
                            selectInput("bar_gene_lists", "Select Gene Lists:",
                                        choices = NULL, selected = NULL, multiple = TRUE),
                            plotlyOutput("plot_bar")
                   ),
                   tabPanel("Violin Plot",
                            selectInput("violin_table", "Select table for Violin Plot:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("violin_col", "Select numeric column:",
                                        choices = NULL, selected = ""),
                            checkboxInput("violin_show_points", "Show all points", value = FALSE),
                            # Multi-select for gene lists:
                            selectInput("violin_gene_lists", "Select Gene Lists:",
                                        choices = NULL, selected = NULL, multiple = TRUE),
                            plotlyOutput("plot_violin")
                   ),
                   tabPanel("Scatter Plot",
                            selectInput("scatter_table_x", "Select table for X:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("scatter_x", "Select X column:",
                                        choices = NULL, selected = ""),
                            selectInput("scatter_table_y", "Select table for Y:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("scatter_y", "Select Y column:",
                                        choices = NULL, selected = ""),
                            plotlyOutput("plot_scatter")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  ### SAVED GENE LISTS UI ###
  output$saved_gene_lists_ui <- renderUI({
    if(length(saved_gene_lists$data) == 0) {
      HTML("<em>No saved gene lists.</em>")
    } else {
      tagList(
        lapply(names(saved_gene_lists$data), function(name) {
          count <- length(saved_gene_lists$data[[name]]$genes)
          btnId <- paste0("apply_", gsub(" ", "_", name))
          fluidRow(
            column(4, strong(name)),
            column(4, paste("Genes:", count)),
            column(4, actionButton(btnId, "Apply Filters", class = "btn-sm"))
          )
        })
      )
    }
  })

  # Create dynamic observers for each saved gene list "Apply Filters" button.
  observe({
    req(saved_gene_lists$data)
    for(name in names(saved_gene_lists$data)) {
      local({
        listName <- name
        btnId <- paste0("apply_", gsub(" ", "_", name))
        observeEvent(input[[btnId]], {
          saved_filters <- saved_gene_lists$data[[listName]]$filters
          for(key in names(saved_filters)) {
            val <- saved_filters[[key]]
            if(is.numeric(val)) {
              updateSliderInput(session, key, value = val)
            } else {
              updateSelectInput(session, key, selected = val)
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  ### CURRENT FILTERS UI ###
  output$current_filters <- renderUI({
    all_inputs <- reactiveValuesToList(input)
    filter_keys <- names(all_inputs)[sapply(names(all_inputs), function(x) {
      any(sapply(individual_tables, function(tbl) {
        startsWith(x, paste0(tbl, "_"))
      }))
    })]
    filters <- all_inputs[filter_keys]
    tables <- unique(sapply(filter_keys, function(x) strsplit(x, "_")[[1]][1]))
    html_out <- ""
    for(tbl in tables) {
      tbl_keys <- filter_keys[startsWith(filter_keys, paste0(tbl, "_"))]
      rows <- sapply(tbl_keys, function(key) {
        colname <- sub(paste0("^", tbl, "_"), "", key)
        value <- filters[[key]]
        if (is.vector(value) && length(value) > 1) {
          value <- paste(value, collapse = ", ")
        }
        paste0("<tr><td style='padding:4px;'><strong>", colname, "</strong></td>",
               "<td style='padding:4px;'>", value, "</td></tr>")
      })
      tbl_html <- paste0("<h4>", tbl, "</h4>",
                         "<table style='width:100%; border-collapse: collapse; border: 1px solid #ccc;'>",
                         "<tr><th style='padding:4px;'>Filter</th><th style='padding:4px;'>Value</th></tr>",
                         paste(rows, collapse = ""), "</table>")
      html_out <- paste(html_out, tbl_html, sep = "<br>")
    }
    HTML(html_out)
  })

  ### HELPER FUNCTIONS & DYNAMIC FILTER UI ###

  get_choices <- function(tbl, col) {
    query <- sprintf("SELECT DISTINCT %s FROM %s", col, tbl)
    vals <- dbGetQuery(con, query)[[col]]
    unique(vals[!is.na(vals)])
  }

  lapply(individual_tables, function(tbl) {
    output[[paste0("filters_", tbl)]] <- renderUI({
      cols <- dbListFields(con, tbl)
      ui_list <- lapply(cols, function(col) {
        input_id <- paste(tbl, col, sep = "_")
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        if (is.numeric(sample_val)) {
          vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
          sliderInput(input_id, label = col,
                      min = min(vals, na.rm = TRUE),
                      max = max(vals, na.rm = TRUE),
                      value = range(vals, na.rm = TRUE))
        } else {
          vals <- get_choices(tbl, col)
          selectInput(input_id, label = col,
                      choices = c("All", vals),
                      selected = "All", multiple = TRUE)
        }
      })
      do.call(tagList, ui_list)
    })
  })

  filtered_data <- function(tbl) {
    reactive({
      cols <- dbListFields(con, tbl)
      conditions <- c()
      params <- list()

      for (col in cols) {
        input_id <- paste(tbl, col, sep = "_")
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        if (is.numeric(sample_val)) {
          slider_val <- input[[input_id]]
          if (!is.null(slider_val)) {
            conditions <- c(conditions, sprintf("%s BETWEEN ? AND ?", col))
            params <- c(params, slider_val[1], slider_val[2])
          }
        } else {
          input_val <- input[[input_id]]
          if (!is.null(input_val) && length(input_val) > 0 && !("All" %in% input_val)) {
            placeholders <- paste(rep("?", length(input_val)), collapse = ", ")
            conditions <- c(conditions, sprintf("%s IN (%s)", col, placeholders))
            params <- c(params, input_val)
          }
        }
      }
      sql <- paste("SELECT * FROM", tbl)
      if (length(conditions) > 0) {
        sql <- paste(sql, "WHERE", paste(conditions, collapse = " AND "))
      }
      dbGetQuery(con, sql, params = params)
    }) |> debounce(5)
  }

  intersected_gene_ids <- reactive({
    filtered_ids <- lapply(individual_tables, function(tbl) {
      df <- filtered_data(tbl)()
      unique(df$GeneID)
    })
    if (length(filtered_ids) == 0) return(character(0))
    Reduce(intersect, filtered_ids)
  })

  output$aggregated_table <- renderReactable({
    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) {
      return(reactable(data.frame(Message = "No matching gene IDs")))
    }
    placeholders <- paste(rep("?", length(gene_ids)), collapse = ", ")
    sql <- paste("SELECT * FROM aggregated WHERE GeneID IN (", placeholders, ")", sep = "")
    df <- dbGetQuery(con, sql, params = gene_ids)
    reactable(df, searchable = TRUE, filterable = TRUE, pagination = TRUE)
  })

  observeEvent(input$bar_table, {
    cols <- dbListFields(con, input$bar_table)
    updateSelectInput(session, "bar_col", choices = c("", cols), selected = "")
  })

  observeEvent(input$violin_table, {
    cols <- dbListFields(con, input$violin_table)
    updateSelectInput(session, "violin_col", choices = c("", cols), selected = "")
  })

  observeEvent(input$scatter_table_x, {
    cols_x <- dbListFields(con, input$scatter_table_x)
    updateSelectInput(session, "scatter_x", choices = c("", cols_x), selected = "")
  })

  observeEvent(input$scatter_table_y, {
    cols_y <- dbListFields(con, input$scatter_table_y)
    updateSelectInput(session, "scatter_y", choices = c("", cols_y), selected = "")
  })

  # Update gene list selectors for bar and violin plots.
  observe({
    choices <- c("Current List", names(saved_gene_lists$data))
    updateSelectInput(session, "bar_gene_lists", choices = choices, selected = choices)
    updateSelectInput(session, "violin_gene_lists", choices = choices, selected = choices)
  })

  noDataPlot <- function() {
    plot_ly() %>%
      layout(title = "No data to plot",
             annotations = list(
               list(text = "No data to plot",
                    showarrow = FALSE,
                    x = 0.5, y = 0.5,
                    xref = "paper", yref = "paper")
             ))
  }

  ### BAR CHART OUTPUT ###
  output$plot_bar <- renderPlotly({
    if (is.null(input$bar_table) || input$bar_table == "" ||
        is.null(input$bar_col) || input$bar_col == "" ||
        is.null(input$bar_gene_lists) || length(input$bar_gene_lists) == 0)
      return(noDataPlot())

    df <- filtered_data(input$bar_table)()
    if (nrow(df) == 0) return(noDataPlot())

    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(noDataPlot())
    df <- df[df$GeneID %in% gene_ids, ]
    if(nrow(df) == 0) return(noDataPlot())

    # Row bind subsets for each selected gene list.
    combined <- do.call(rbind, lapply(input$bar_gene_lists, function(listName) {
      if(listName == "Current List") {
        subset_genes <- intersected_gene_ids()
      } else {
        subset_genes <- saved_gene_lists$data[[listName]]$genes
      }
      df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
      if(nrow(df_subset)==0) return(NULL)
      df_subset$gene_list <- listName
      df_subset
    }))
    if(is.null(combined) || nrow(combined)==0) return(noDataPlot())

    # For convenience, create a new column "value" holding the selected column's values.
    combined$value <- combined[[input$bar_col]]

    if(is.numeric(combined$value)) {
      p <- plot_ly(data = combined, x = ~value, color = ~gene_list, type = "histogram",
                   histnorm = ifelse(input$bar_y_type=="Percentage", "percent", ""))
    } else {
      # For non-numeric columns, compute counts per group.
      counts <- combined %>%
        group_by(gene_list, value = .data[[input$bar_col]]) %>%
        summarise(count = n(), .groups = "drop")
      if(input$bar_y_type=="Percentage") {
        counts <- counts %>% group_by(gene_list) %>%
          mutate(percentage = count/sum(count)*100) %>% ungroup()
        p <- plot_ly(data = counts, x = ~value, y = ~percentage, color = ~gene_list, type = "bar")
      } else {
        p <- plot_ly(data = counts, x = ~value, y = ~count, color = ~gene_list, type = "bar")
      }
    }

    p <- layout(p, barmode = "group",
                title = paste("Bar Chart of", input$bar_col),
                xaxis = list(title = input$bar_col),
                yaxis = list(title = ifelse(input$bar_y_type=="Percentage", "Percentage", "Count")))
    p
  })

  ### VIOLIN PLOT OUTPUT ###
  output$plot_violin <- renderPlotly({
    if (is.null(input$violin_table) || input$violin_table == "" ||
        is.null(input$violin_col) || input$violin_col == "" ||
        is.null(input$violin_gene_lists) || length(input$violin_gene_lists) == 0)
      return(noDataPlot())

    df <- filtered_data(input$violin_table)()
    if (nrow(df) == 0) return(noDataPlot())

    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(noDataPlot())
    df <- df[df$GeneID %in% gene_ids, ]
    if(nrow(df)==0) return(noDataPlot())

    combined <- do.call(rbind, lapply(input$violin_gene_lists, function(listName) {
      if(listName=="Current List") {
        subset_genes <- intersected_gene_ids()
      } else {
        subset_genes <- saved_gene_lists$data[[listName]]$genes
      }
      df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
      if(nrow(df_subset)==0) return(NULL)
      df_subset$gene_list <- listName
      df_subset
    }))
    if(is.null(combined) || nrow(combined)==0) return(noDataPlot())

    combined$value <- combined[[input$violin_col]]
    if(!is.numeric(combined$value)) {
      return(plot_ly() %>% layout(title = "Error",
                                  annotations = list(
                                    list(text = "Can't use factor for numeric plot",
                                         showarrow = FALSE,
                                         x = 0.5, y = 0.5,
                                         xref = "paper", yref = "paper")
                                  )))
    }

    # Create hover text for each row.
    combined$hover_text <- paste("Gene: ", combined$GeneID, "<br>",
                                 input$violin_col, ": ", combined$value)

    p <- plot_ly(data = combined, y = ~value, color = ~gene_list, type = "violin",
                 box = list(visible = TRUE),
                 meanline = list(visible = TRUE),
                 points = ifelse(input$violin_show_points, "all", "outliers"),
                 text = ~hover_text,
                 hoverinfo = "text")

    p <- layout(p, title = paste("Violin Plot of", input$violin_col),
                yaxis = list(title = input$violin_col))
    p
  })

  ### SCATTER PLOT OUTPUT (unchanged) ###
  output$plot_scatter <- renderPlotly({
    if (is.null(input$scatter_table_x) || input$scatter_table_x == "" ||
        is.null(input$scatter_table_y) || input$scatter_table_y == "" ||
        is.null(input$scatter_x) || input$scatter_x == "" ||
        is.null(input$scatter_y) || input$scatter_y == "")
      return(noDataPlot())

    gene_ids <- intersected_gene_ids()
    if(length(gene_ids)==0) return(noDataPlot())

    if (input$scatter_table_x == input$scatter_table_y) {
      df_joint <- filtered_data(input$scatter_table_x)()
      df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
      if(nrow(df_joint)==0) return(noDataPlot())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
        return(noDataPlot())
      xvar <- df_joint[[input$scatter_x]]
      yvar <- df_joint[[input$scatter_y]]
      geneid <- df_joint$GeneID
    } else {
      df_x <- filtered_data(input$scatter_table_x)()
      df_y <- filtered_data(input$scatter_table_y)()
      df_x <- df_x[df_x$GeneID %in% gene_ids, ]
      df_y <- df_y[df_y$GeneID %in% gene_ids, ]
      df_joint <- inner_join(df_x, df_y, by = "GeneID")
      if(nrow(df_joint)==0) return(noDataPlot())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
        return(noDataPlot())
      xvar <- df_joint[[input$scatter_x]]
      yvar <- df_joint[[input$scatter_y]]
      geneid <- df_joint$GeneID
    }

    if (is.factor(xvar)) xvar <- as.numeric(xvar) + rnorm(length(xvar), 0, 0.1)
    if (is.factor(yvar)) yvar <- as.numeric(yvar) + rnorm(length(yvar), 0, 0.1)

    p <- plot_ly(data = df_joint,
                 x = ~xvar,
                 y = ~yvar,
                 type = "scatter",
                 mode = "markers",
                 text = ~paste("GeneID:", geneid),
                 hoverinfo = "text+x+y") %>%
      layout(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y),
             xaxis = list(title = input$scatter_x),
             yaxis = list(title = input$scatter_y))
    p
  })

  ### SAVE GENE LIST FUNCTIONALITY ###
  observeEvent(input$save_gene_list, {
    showModal(modalDialog(
      title = "Save Gene List",
      textInput("gene_list_name", "Enter a name for this gene list:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Save")
      )
    ))
  })

  observeEvent(input$confirm_save, {
    req(input$gene_list_name)
    current_genes <- intersected_gene_ids()
    current_filters <- reactiveValuesToList(input)
    filter_keys <- names(current_filters)[sapply(names(current_filters), function(x) {
      any(sapply(individual_tables, function(tbl) {
        startsWith(x, paste0(tbl, "_"))
      }))
    })]
    saved_filters <- current_filters[filter_keys]
    saved_gene_lists$data[[input$gene_list_name]] <- list(
      genes = current_genes,
      filters = saved_filters
    )
    removeModal()
  })

}

shinyApp(ui, server)
