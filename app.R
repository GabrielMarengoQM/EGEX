library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(reactable)
library(plotly)

# Connect to DuckDB
con <- dbConnect(duckdb(), "mydb.duckdb")
# Get all table names; exclude "aggregated" for the individual table tabs.
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "aggregated")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Dynamic DuckDB Explorer"),
  tabsetPanel(
    # First top-level tab: Aggregated Data (filtered by intersection)
    tabPanel("Aggregated Data",
             reactableOutput("aggregated_table")
    ),
    # Second top-level tab: Individual Tables (with filters and plots)
    tabPanel("Individual Tables",
             # Nested tabset panel for each individual table.
             do.call(tabsetPanel,
                     lapply(individual_tables, function(tbl) {
                       tabPanel(tbl,
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput(paste0("filters_", tbl))
                                  ),
                                  mainPanel(
                                    reactableOutput(paste0(tbl, "_table")),
                                    br(),
                                    uiOutput(paste0("plot_selector_", tbl)),
                                    plotlyOutput(paste0(tbl, "_plot"))
                                  )
                                )
                       )
                     })
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
        # Sample value to determine type.
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        if (is.numeric(sample_val)) {
          # Numeric: sliderInput.
          vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
          sliderInput(input_id, label = col,
                      min = min(vals, na.rm = TRUE),
                      max = max(vals, na.rm = TRUE),
                      value = range(vals, na.rm = TRUE))
        } else {
          # Non-numeric: multi-select input.
          vals <- get_choices(tbl, col)
          selectInput(input_id, label = col,
                      choices = c("All", vals),
                      selected = "All", multiple = TRUE)
        }
      })
      do.call(tagList, ui_list)
    })
  })

  # Generate dynamic plot column selector for each individual table.
  lapply(individual_tables, function(tbl) {
    output[[paste0("plot_selector_", tbl)]] <- renderUI({
      cols <- dbListFields(con, tbl)
      selectInput(inputId = paste0(tbl, "_plotcol"),
                  label = "Select Column for Plot:",
                  choices = cols,
                  selected = cols[1])
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
    }) |> debounce(500)
  }

  # Render individual tables and plots.
  lapply(individual_tables, function(tbl) {
    output[[paste0(tbl, "_table")]] <- renderReactable({
      df <- filtered_data(tbl)()
      reactable(df, searchable = TRUE, pagination = TRUE)
    })
    output[[paste0(tbl, "_plot")]] <- renderPlotly({
      df <- filtered_data(tbl)()
      plotcol <- input[[paste0(tbl, "_plotcol")]]
      if (is.null(plotcol) || nrow(df) == 0) return(NULL)
      if (is.numeric(df[[plotcol]])) {
        plot_ly(df, y = ~df[[plotcol]], type = "box") %>%
          layout(title = paste("Box Plot of", plotcol),
                 yaxis = list(title = plotcol))
      } else {
        counts <- as.data.frame(table(df[[plotcol]], useNA = "ifany"))
        names(counts) <- c("value", "count")
        counts$value[is.na(counts$value)] <- "NA"
        plot_ly(counts, x = ~value, y = ~count, type = "bar") %>%
          layout(title = paste("Bar Chart of", plotcol),
                 xaxis = list(title = plotcol),
                 yaxis = list(title = "Count"))
      }
    })
  })

  # Reactive: Compute intersection of GeneIDs from all individual tables.
  intersected_gene_ids <- reactive({
    filtered_ids <- lapply(individual_tables, function(tbl) {
      df <- filtered_data(tbl)()
      unique(df$GeneID)
    })
    if (length(filtered_ids) == 0) return(character(0))
    Reduce(intersect, filtered_ids)
  })

  # Render the Aggregated Data tab.
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

}

shinyApp(ui, server)
