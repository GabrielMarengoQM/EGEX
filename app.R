library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(reactable)
library(plotly)

# Connect to DuckDB
con <- dbConnect(duckdb(), "mydb.duckdb")
tables <- dbListTables(con)

# UI Definition: Dynamic tabs for each table.
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Dynamic DuckDB SQL-powered Explorer"),
  do.call(tabsetPanel,
          lapply(tables, function(tbl) {
            tabPanel(tbl,
                     sidebarLayout(
                       sidebarPanel(
                         # Dynamic filters for the current table
                         uiOutput(paste0("filters_", tbl))
                       ),
                       mainPanel(
                         # Display the filtered table
                         reactableOutput(paste0(tbl, "_table")),
                         # Dynamic plot column selector
                         uiOutput(paste0("plot_selector_", tbl)),
                         # Display the corresponding plot
                         plotlyOutput(paste0(tbl, "_plot"))
                       )
                     )
            )
          })
  )
)

# Server Logic
server <- function(input, output, session) {

  # Helper function to get distinct values for a given table/column.
  get_choices <- function(tbl, col) {
    # Returns unique values from the specified column.
    dbGetQuery(con, paste("SELECT DISTINCT", col, "FROM", tbl))[[col]]
  }

  # For each table, dynamically generate UI for filters and plot selector.
  lapply(tables, function(tbl) {

    # Generate dynamic filters based on the table's columns.
    output[[paste0("filters_", tbl)]] <- renderUI({
      cols <- dbListFields(con, tbl)
      do.call(tagList, lapply(cols, function(col) {
        vals <- get_choices(tbl, col)
        input_id <- paste(tbl, col, sep = "_")
        if (is.numeric(vals)) {
          sliderInput(input_id, label = col,
                      min = min(vals, na.rm = TRUE),
                      max = max(vals, na.rm = TRUE),
                      value = range(vals, na.rm = TRUE))
        } else {
          selectInput(input_id, label = col,
                      choices = c("All", vals),
                      selected = "All",
                      multiple = TRUE)
        }
      }))
    })

    # Generate a dynamic plot column selector for the current table.
    output[[paste0("plot_selector_", tbl)]] <- renderUI({
      cols <- dbListFields(con, tbl)
      selectInput(inputId = paste0(tbl, "_plotcol"),
                  label = "Select Plot Column",
                  choices = cols,
                  selected = cols[1])
    })

    # Function to build and run a dynamic SQL query for filtering.
    filtered_data <- function(tbl) {
      cols <- dbListFields(con, tbl)
      conditions <- c()
      params <- list()
      for (col in cols) {
        input_val <- input[[paste(tbl, col, sep = "_")]]
        if (!is.null(input_val) && !"All" %in% input_val) {
          if (is.numeric(input_val)) {
            conditions <- c(conditions, sprintf("%s BETWEEN ? AND ?", col))
            params <- c(params, input_val[1], input_val[2])
          } else {
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
    }

    # Render the filtered table using reactable.
    output[[paste0(tbl, "_table")]] <- renderReactable({
      # Run the dynamically constructed query.
      df <- filtered_data(tbl)
      reactable(df, searchable = TRUE, pagination = TRUE)
    })

    # Render a plot based on the selected plot column.
    output[[paste0(tbl, "_plot")]] <- renderPlotly({
      # Get the filtered data.
      df <- filtered_data(tbl)
      plotcol <- input[[paste0(tbl, "_plotcol")]]
      if (is.null(plotcol) || nrow(df) == 0) return(NULL)
      if (is.numeric(df[[plotcol]])) {
        # For numeric columns: box plot.
        plot_ly(df, y = ~df[[plotcol]], type = "box") %>%
          layout(title = paste("Box Plot of", plotcol))
      } else {
        # For non-numeric columns: bar chart of value counts.
        counts <- as.data.frame(table(df[[plotcol]]))
        names(counts) <- c("value", "count")
        plot_ly(counts, x = ~value, y = ~count, type = "bar") %>%
          layout(title = paste("Bar Chart of", plotcol),
                 xaxis = list(title = plotcol),
                 yaxis = list(title = "Count"))
      }
    })

  })
}

shinyApp(ui, server)
