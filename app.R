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

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Dynamic DuckDB Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Use bslib accordion with two panels:
      accordion(
        accordion_panel("Current Filters",
                        verbatimTextOutput("current_filters")),
        accordion_panel("Filter Controls",
                        # For each table, display its filter UI.
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
                            plotlyOutput("plot_bar")
                   ),
                   tabPanel("Violin Plot",
                            selectInput("violin_table", "Select table for Violin Plot:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("violin_col", "Select numeric column:",
                                        choices = NULL, selected = ""),
                            checkboxInput("violin_show_points", "Show all points", value = FALSE),
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

  # Display only filter-related inputs (keys starting with one of the table names).
  output$current_filters <- renderPrint({
    all_inputs <- reactiveValuesToList(input)
    filter_keys <- names(all_inputs)[sapply(names(all_inputs), function(x) {
      any(sapply(individual_tables, function(tbl) {
        startsWith(x, paste0(tbl, "_"))
      }))
    })]
    all_inputs[filter_keys]
  })

  # Helper: Get distinct non-NA values for a given table/column.
  get_choices <- function(tbl, col) {
    query <- sprintf("SELECT DISTINCT %s FROM %s", col, tbl)
    vals <- dbGetQuery(con, query)[[col]]
    unique(vals[!is.na(vals)])
  }

  # Generate dynamic filter UI for each individual table.
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

  # Build reactive filtered data for a given table.
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

  # Compute intersection of GeneIDs from all individual tables.
  intersected_gene_ids <- reactive({
    filtered_ids <- lapply(individual_tables, function(tbl) {
      df <- filtered_data(tbl)()
      unique(df$GeneID)
    })
    if (length(filtered_ids) == 0) return(character(0))
    Reduce(intersect, filtered_ids)
  })

  # Render the Aggregated Data table using the intersected GeneIDs.
  output$aggregated_table <- renderReactable({
    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) {
      return(reactable(data.frame(Message = "No matching gene IDs")))
    }
    placeholders <- paste(rep("?", length(gene_ids)), collapse = ", ")
    sql <- paste("SELECT * FROM aggregated WHERE GeneID IN (", placeholders, ")", sep = "")
    df <- dbGetQuery(con, sql, params = gene_ids)
    reactable(df, searchable = TRUE, pagination = TRUE)
  })

  # --- Update Bar Chart column selector ---
  observeEvent(input$bar_table, {
    cols <- dbListFields(con, input$bar_table)
    updateSelectInput(session, "bar_col", choices = c("", cols), selected = "")
  })

  # --- Update Violin Plot column selector ---
  observeEvent(input$violin_table, {
    cols <- dbListFields(con, input$violin_table)
    updateSelectInput(session, "violin_col", choices = c("", cols), selected = "")
  })

  # --- Update Scatter Plot X column selector ---
  observeEvent(input$scatter_table_x, {
    cols_x <- dbListFields(con, input$scatter_table_x)
    updateSelectInput(session, "scatter_x", choices = c("", cols_x), selected = "")
  })

  # --- Update Scatter Plot Y column selector ---
  observeEvent(input$scatter_table_y, {
    cols_y <- dbListFields(con, input$scatter_table_y)
    updateSelectInput(session, "scatter_y", choices = c("", cols_y), selected = "")
  })

  # Helper function for "No data to plot" message.
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

  # --- Plot: Bar Chart ---
  output$plot_bar <- renderPlotly({
    if (is.null(input$bar_table) || input$bar_table == "" ||
        is.null(input$bar_col) || input$bar_col == "") return(noDataPlot())

    df <- filtered_data(input$bar_table)()
    if (nrow(df) == 0) return(noDataPlot())

    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(noDataPlot())
    df <- df[df$GeneID %in% gene_ids, ]
    if (nrow(df) == 0) return(noDataPlot())

    var <- df[[input$bar_col]]
    if (is.numeric(var)) {
      if (input$bar_y_type == "Percentage") {
        p <- plot_ly(data = df, x = ~var, type = "histogram", histnorm = "percent") %>%
          layout(title = paste("Histogram of", input$bar_col),
                 xaxis = list(title = input$bar_col),
                 yaxis = list(title = "Percentage"))
      } else {
        p <- plot_ly(data = df, x = ~var, type = "histogram") %>%
          layout(title = paste("Histogram of", input$bar_col),
                 xaxis = list(title = input$bar_col),
                 yaxis = list(title = "Count"))
      }
    } else {
      counts <- as.data.frame(table(var, useNA = "ifany"))
      names(counts) <- c("value", "count")
      if (input$bar_y_type == "Percentage") {
        counts$percentage <- counts$count / sum(counts$count) * 100
        p <- plot_ly(data = counts, x = ~value, y = ~percentage, type = "bar") %>%
          layout(title = paste("Bar Chart of", input$bar_col),
                 xaxis = list(title = input$bar_col),
                 yaxis = list(title = "Percentage"))
      } else {
        p <- plot_ly(data = counts, x = ~value, y = ~count, type = "bar") %>%
          layout(title = paste("Bar Chart of", input$bar_col),
                 xaxis = list(title = input$bar_col),
                 yaxis = list(title = "Count"))
      }
    }
    p
  })

  # --- Plot: Violin Plot ---
  output$plot_violin <- renderPlotly({
    if (is.null(input$violin_table) || input$violin_table == "" ||
        is.null(input$violin_col) || input$violin_col == "") {
      return(noDataPlot())
    }

    df <- filtered_data(input$violin_table)()
    if (nrow(df) == 0) return(noDataPlot())

    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(noDataPlot())
    df <- df[df$GeneID %in% gene_ids, ]
    if(nrow(df)==0) return(noDataPlot())

    var <- df[[input$violin_col]]
    if (!is.numeric(var)) {
      return(plot_ly() %>% layout(title = "Error",
                                  annotations = list(
                                    list(text = "Can't use factor for numeric plot",
                                         showarrow = FALSE,
                                         x = 0.5, y = 0.5,
                                         xref = "paper", yref = "paper")
                                  )))
    }

    # Create hover text that displays GeneID and the selected column's value.
    df$hover_text <- paste("Gene: ", df$GeneID, "<br>",
                           input$violin_col, ": ", df[[input$violin_col]])

    p <- plot_ly(data = df,
                 y = as.formula(paste0("~", input$violin_col)),
                 type = "violin",
                 box = list(visible = TRUE),
                 meanline = list(visible = TRUE),
                 points = ifelse(input$violin_show_points, "all", "outliers"),
                 text = ~hover_text,
                 hoverinfo = "text") %>%
      layout(title = paste("Violin Plot of", input$violin_col),
             yaxis = list(title = input$violin_col))
    p
  })

  # --- Plot: Scatter Plot ---
  output$plot_scatter <- renderPlotly({
    if (is.null(input$scatter_table_x) || input$scatter_table_x == "" ||
        is.null(input$scatter_table_y) || input$scatter_table_y == "" ||
        is.null(input$scatter_x) || input$scatter_x == "" ||
        is.null(input$scatter_y) || input$scatter_y == "") {
      return(noDataPlot())
    }

    gene_ids <- intersected_gene_ids()
    if(length(gene_ids)==0) return(noDataPlot())

    if (input$scatter_table_x == input$scatter_table_y) {
      df_joint <- filtered_data(input$scatter_table_x)()
      df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
      if(nrow(df_joint)==0) return(noDataPlot())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint))){
        return(noDataPlot())
      }
      xvar <- df_joint[[input$scatter_x]]
      yvar <- df_joint[[input$scatter_y]]
      # Retain GeneID for hover text.
      geneid <- df_joint$GeneID
    } else {
      df_x <- filtered_data(input$scatter_table_x)()
      df_y <- filtered_data(input$scatter_table_y)()
      df_x <- df_x[df_x$GeneID %in% gene_ids, ]
      df_y <- df_y[df_y$GeneID %in% gene_ids, ]
      df_joint <- inner_join(df_x, df_y, by = "GeneID")
      if(nrow(df_joint)==0) return(noDataPlot())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint))){
        return(noDataPlot())
      }
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

}

shinyApp(ui, server)
