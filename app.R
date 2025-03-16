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
      # Display filters for each individual table
      lapply(individual_tables, function(tbl) {
        tagList(
          h4(tbl),
          uiOutput(paste0("filters_", tbl))
        )
      })
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
                            plotlyOutput("plot_violin")
                   ),
                   tabPanel("Scatter Plot",
                            h4("For X Variable:"),
                            selectInput("scatter_table_x", "Select table for X:",
                                        choices = individual_tables,
                                        selected = individual_tables[1]),
                            selectInput("scatter_x", "Select X column:",
                                        choices = NULL, selected = ""),
                            h4("For Y Variable:"),
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

  # Function: Build reactive filtered data for a given table.
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

  # Reactive: Compute intersection of GeneIDs from all individual tables.
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

  # --- Plot: Bar Chart ---
  output$plot_bar <- renderPlotly({
    if (is.null(input$bar_table) || input$bar_table == "" ||
        is.null(input$bar_col) || input$bar_col == "") return(plotly_empty())

    df <- filtered_data(input$bar_table)()
    if (nrow(df) == 0) return(plotly_empty())

    # Filter by intersected GeneIDs.
    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(plotly_empty())
    df <- df[df$GeneID %in% gene_ids, ]
    if(nrow(df)==0) return(plotly_empty())

    var <- df[[input$bar_col]]
    if (is.numeric(var)) {
      if(input$bar_y_type == "Percentage") {
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
      if(input$bar_y_type == "Percentage") {
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
      return(plotly_empty())
    }

    df <- filtered_data(input$violin_table)()
    if (nrow(df) == 0) return(plotly_empty())

    # Filter by intersected GeneIDs.
    gene_ids <- intersected_gene_ids()
    if (length(gene_ids) == 0) return(plotly_empty())
    df <- df[df$GeneID %in% gene_ids, ]
    if(nrow(df)==0) return(plotly_empty())

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

    p <- plot_ly(data = df, y = as.formula(paste0("~", input$violin_col)),
                 type = "violin", box = list(visible = TRUE),
                 meanline = list(visible = TRUE)) %>%
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
      return(plotly_empty())
    }

    # Get the intersected GeneIDs from all filters.
    gene_ids <- intersected_gene_ids()
    if(length(gene_ids)==0) return(plotly_empty())

    # If the X and Y tables are the same, use that table's filtered data.
    if (input$scatter_table_x == input$scatter_table_y) {
      df_joint <- filtered_data(input$scatter_table_x)()
      df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
      if(nrow(df_joint)==0) return(plotly_empty())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint))){
        return(plotly_empty())
      }
      xvar <- df_joint[[input$scatter_x]]
      yvar <- df_joint[[input$scatter_y]]
    } else {
      # If different tables, join on GeneID.
      df_x <- filtered_data(input$scatter_table_x)()
      df_y <- filtered_data(input$scatter_table_y)()
      df_x <- df_x[df_x$GeneID %in% gene_ids, ]
      df_y <- df_y[df_y$GeneID %in% gene_ids, ]
      df_joint <- inner_join(df_x, df_y, by = "GeneID")
      if(nrow(df_joint)==0) return(plotly_empty())
      if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint))){
        return(plotly_empty())
      }
      xvar <- df_joint[[input$scatter_x]]
      yvar <- df_joint[[input$scatter_y]]
    }

    if (is.factor(xvar)) xvar <- as.numeric(xvar) + rnorm(length(xvar), 0, 0.1)
    if (is.factor(yvar)) yvar <- as.numeric(yvar) + rnorm(length(yvar), 0, 0.1)

    p <- plot_ly(x = xvar, y = yvar, type = "scatter", mode = "markers") %>%
      layout(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y),
             xaxis = list(title = input$scatter_x),
             yaxis = list(title = input$scatter_y))
    p
  })

}

shinyApp(ui, server)
