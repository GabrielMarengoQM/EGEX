# Load required libraries
library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(plotly)
library(reactable)
library(dbplyr)
library(arrow)

#------------------------------------------------
# Persistent Database Setup: Connect to a DuckDB file
#------------------------------------------------
# Connect to a persistent DuckDB file instead of using :memory:
con <- dbConnect(duckdb::duckdb(), dbdir = "mydb.duckdb", read_only = FALSE)
#------------------------------------------------
# Setup Database: Load Parquet files into DuckDB (if not already loaded)
#------------------------------------------------
# setupDatabase <- function(conn) {
#   if (!("genes" %in% dbListTables(conn))) {
#     genes_df <- read_parquet("genes.parquet")
#     dbWriteTable(conn, "genes", genes_df, overwrite = TRUE)
#   }
#   if (!("metrics" %in% dbListTables(conn))) {
#     metrics_df <- read_parquet("metrics.parquet")
#     dbWriteTable(conn, "metrics", metrics_df, overwrite = TRUE)
#   }
#   if (!("pathways" %in% dbListTables(conn))) {
#     pathways_df <- read_parquet("pathways.parquet")
#     dbWriteTable(conn, "pathways", pathways_df, overwrite = TRUE)
#   }
# }
#
# setupDatabase(con)

#------------------------------------------------
# Static Schema Retrieval: Retrieve the DB schema once at startup.
#------------------------------------------------
static_schema <- {
  tables <- dbListTables(con)
  lapply(tables, function(tbl) {
    list(table = tbl, fields = dbListFields(con, tbl))
  })
}

#------------------------------------------------
# Helper Function: Build Aggregated Query (unchanged)
#------------------------------------------------
buildAggregatedQuery <- function(schema) {
  primary <- NULL
  for (tbl_obj in schema) {
    if (tbl_obj$table == "genes") {
      primary <- tbl_obj
      break
    }
  }
  if (is.null(primary)) primary <- schema[[1]]

  subqueries <- list()
  for (tbl_obj in schema) {
    tbl <- tbl_obj$table
    fields <- tbl_obj$fields
    other_fields <- setdiff(fields, "GeneID")
    alias <- paste0(tbl, "_agg")
    if (length(other_fields) > 0) {
      agg_fields <- sapply(other_fields, function(col) {
        paste0("LISTAGG(DISTINCT ", col, ", ', ') AS ", col)
      })
      agg_fields_str <- paste(agg_fields, collapse = ", ")
      subquery_text <- sprintf("(SELECT GeneID, %s FROM %s GROUP BY GeneID) AS %s",
                               agg_fields_str, tbl, alias)
    } else {
      subquery_text <- sprintf("(SELECT GeneID FROM %s GROUP BY GeneID) AS %s", tbl, alias)
    }
    subqueries[[tbl]] <- list(alias = alias, text = subquery_text)
  }

  primary_alias <- subqueries[[primary$table]]$alias
  final_query <- sprintf("SELECT %s.GeneID", primary_alias)
  for (tbl in names(subqueries)) {
    final_query <- paste(final_query, ", ", subqueries[[tbl]]$alias, ".*", sep = "")
  }
  final_query <- paste(final_query, "FROM", subqueries[[primary$table]]$text)
  for (tbl in names(subqueries)) {
    if (tbl != primary$table) {
      final_query <- paste(final_query, sprintf("LEFT JOIN %s ON %s.GeneID = %s.GeneID",
                                                subqueries[[tbl]]$text, primary_alias, subqueries[[tbl]]$alias))
    }
  }
  final_query
}

#------------------------------------------------
# Helper function: Wrap output in a card with full_screen = TRUE.
#------------------------------------------------
cardOutput <- function(title, output_ui) {
  card(
    full_screen = TRUE,
    card_header(title),
    card_body(output_ui)
  )
}

#------------------------------------------------
# Function to build dynamic tabs for individual tables using lazy tables.
#------------------------------------------------
buildDynamicTabs <- function(schema) {
  tabs <- lapply(schema, function(tbl_obj) {
    tbl <- tbl_obj$table
    fields <- tbl_obj$fields
    data_sample <- tbl(con, tbl) %>% head(1000) %>% collect()

    filterUIs <- lapply(fields, function(col) {
      if (is.numeric(data_sample[[col]])) {
        min_val <- min(data_sample[[col]], na.rm = TRUE)
        max_val <- max(data_sample[[col]], na.rm = TRUE)
        sliderInput(
          inputId = paste0("filter_", tbl, "_", col),
          label = col,
          min = min_val,
          max = max_val,
          value = c(min_val, max_val)
        )
      } else {
        choices <- unique(data_sample[[col]])
        selectInput(
          inputId = paste0("filter_", tbl, "_", col),
          label = col,
          choices = c("All", choices),
          selected = "All",
          multiple = TRUE
        )
      }
    })

    plotSelect <- selectInput(
      inputId = paste0("plotcol_", tbl),
      label = "Select Column for Plot",
      choices = fields
    )

    tabPanel(
      title = tbl,
      fluidRow(
        column(
          width = 3,
          h4("Filters"),
          tagList(filterUIs),
          plotSelect
        ),
        column(
          width = 9,
          fluidRow(
            column(
              width = 4,
              cardOutput("Data Table", reactableOutput(paste0("table_", tbl)))
            ),
            column(
              width = 4,
              cardOutput("Numeric Summary", reactableOutput(paste0("numtable_", tbl)))
            ),
            column(
              width = 4,
              cardOutput("Plot", plotlyOutput(paste0("plot_", tbl)))
            )
          )
        )
      )
    )
  })
  do.call(tabsetPanel, tabs)
}

#------------------------------------------------
# UI Definition with bslib Theme ("lux") and custom CSS for reactable outputs
#------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  tags$head(
    tags$style(HTML("
      .reactable {
        background-color: white !important;
        color: black !important;
      }
      .reactable .rt-thead, .reactable .rt-tbody {
        background-color: white !important;
        color: black !important;
      }
    "))
  ),
  titlePanel("DuckDB-Powered DB Explorer (Persistent Parquet Import)"),
  mainPanel(
    tabsetPanel(
      tabPanel("Individual Tables", buildDynamicTabs(static_schema)),
      tabPanel("Aggregated Data",
               fluidRow(
                 column(width = 8,
                        cardOutput("Aggregated Table (Intersection of filtered GeneIDs)",
                                   reactableOutput("aggregated_table"))
                 ),
                 column(width = 4,
                        cardOutput("Applied Filters",
                                   reactableOutput("applied_filters_table"))
                 )
               )
      ),
      tabPanel("Download Data",
               fluidRow(
                 column(width = 12,
                        h4("Download Filtered Tables (including aggregated) as ZIP"),
                        downloadButton("downloadData", "Download ZIP"))
               )
      )
    )
  )
)

#------------------------------------------------
# Server Definition
#------------------------------------------------
server <- function(input, output, session) {
  filteredDataList <- list()

  lapply(static_schema, function(tbl_obj) {
    tbl <- tbl_obj$table
    fields <- tbl_obj$fields
    local({
      local_tbl <- tbl

      filteredData <- reactive({
        lazy_data <- tbl(con, local_tbl)
        for (col in fields) {
          filterId <- paste0("filter_", local_tbl, "_", col)
          value <- input[[filterId]]
          if (!is.null(value)) {
            sample_vals <- lazy_data %>% head(1000) %>% collect()
            if (is.numeric(sample_vals[[col]])) {
              rng <- value
              lazy_data <- lazy_data %>% filter(.data[[col]] >= !!rng[1], .data[[col]] <= !!rng[2])
            } else {
              if (!("All" %in% value)) {
                lazy_data <- lazy_data %>% filter(.data[[col]] %in% value)
              }
            }
          }
        }
        lazy_data %>% collect()
      })

      filteredDataList[[local_tbl]] <<- filteredData

      output[[paste0("table_", local_tbl)]] <- renderReactable({
        reactable(filteredData())
      })

      output[[paste0("numtable_", local_tbl)]] <- renderReactable({
        data <- filteredData()
        num_data <- data %>% select_if(is.numeric)
        if (ncol(num_data) == 0) {
          reactable(data.frame(Message = "No numeric columns"))
        } else {
          stats <- data.frame(
            Column = names(num_data),
            Mean = sapply(num_data, mean, na.rm = TRUE),
            Median = sapply(num_data, median, na.rm = TRUE),
            SD = sapply(num_data, sd, na.rm = TRUE),
            Min = sapply(num_data, min, na.rm = TRUE),
            Max = sapply(num_data, max, na.rm = TRUE),
            check.names = FALSE
          )
          reactable(stats)
        }
      })

      output[[paste0("plot_", local_tbl)]] <- renderPlotly({
        data <- filteredData()
        plotColId <- paste0("plotcol_", local_tbl)
        plotcol <- input[[plotColId]]
        req(plotcol)
        if (is.numeric(data[[plotcol]])) {
          p <- ggplot(data, aes_string(x = plotcol)) +
            geom_histogram(bins = 30, fill = "grey", color = "black") +
            labs(title = paste("Histogram of", plotcol, "in", local_tbl),
                 x = plotcol, y = "Count")
          ggplotly(p)
        } else {
          data[[plotcol]] <- as.factor(data[[plotcol]])
          agg <- data %>% group_by(!!sym(plotcol)) %>% summarise(count = n(), .groups = "drop")
          p <- ggplot(agg, aes_string(x = plotcol, y = "count")) +
            geom_bar(stat = "identity", fill = "grey", color = "black") +
            labs(title = paste("Counts of", plotcol, "in", local_tbl),
                 x = plotcol, y = "Count")
          ggplotly(p)
        }
      })
    })
  })

  aggregatedGeneIDs <- reactive({
    ids_list <- lapply(names(filteredDataList), function(tbl) {
      unique(filteredDataList[[tbl]]()$GeneID)
    })
    Reduce(intersect, ids_list)
  })

  output$aggregated_table <- renderReactable({
    query <- buildAggregatedQuery(static_schema)
    aggData <- dbGetQuery(con, query)
    valid_ids <- aggregatedGeneIDs()
    reactable(aggData[aggData$GeneID %in% valid_ids, ])
  })

  appliedFilters <- reactive({
    filters_list <- lapply(static_schema, function(tbl_obj) {
      tbl <- tbl_obj$table
      fields <- tbl_obj$fields
      df <- data.frame(Table = tbl, Column = fields, stringsAsFactors = FALSE)
      df$Filter <- sapply(fields, function(col) {
        value <- input[[paste0("filter_", tbl, "_", col)]]
        if (is.null(value)) return("None")
        if (is.numeric(value)) {
          paste(value, collapse = " to ")
        } else {
          paste(value, collapse = ", ")
        }
      })
      df
    })
    do.call(rbind, filters_list)
  })

  output$applied_filters_table <- renderReactable({
    reactable(appliedFilters())
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("filtered_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))

      for (tbl in names(filteredDataList)) {
        data <- filteredDataList[[tbl]]()
        write.csv(data, file = paste0("table_", tbl, ".csv"), row.names = FALSE)
      }
      query <- buildAggregatedQuery(static_schema)
      aggData <- dbGetQuery(con, query)
      valid_ids <- aggregatedGeneIDs()
      aggData <- aggData[aggData$GeneID %in% valid_ids, ]
      write.csv(aggData, file = "aggregated_table.csv", row.names = FALSE)

      zip(file, files = list.files(tmpdir, pattern = "table_|aggregated_table\\.csv"))
    },
    contentType = "application/zip"
  )
}

#------------------------------------------------
# Run the Shiny App
#------------------------------------------------
shinyApp(ui, server)
