library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(reactable)

con <- dbConnect(duckdb(), "mydb.duckdb")

tables <- dbListTables(con)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Dynamic DuckDB Explorer"),
  do.call(tabsetPanel,
          lapply(tables, function(tbl) {
            tabPanel(tbl,
                     sidebarLayout(
                       sidebarPanel(
                         uiOutput(paste0("filters_", tbl))
                       ),
                       mainPanel(
                         reactableOutput(paste0("table_", tbl))
                       )
                     )
            )
          })
  )
)

server <- function(input, output, session) {

  # Dynamically generate filters for each table
  lapply(tables, function(tbl) {
    output[[paste0("filters_", tbl)]] <- renderUI({
      cols <- dbListFields(con, tbl)
      do.call(tagList, lapply(cols, function(col) {
        vals <- dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]]
        input_id <- paste(tbl, col, sep = "_")
        if (is.numeric(vals)) {
          sliderInput(input_id, col, min(vals), max(vals), value = range(vals))
        } else {
          selectInput(input_id, col, choices = c("All", vals), selected = "All", multiple = TRUE)
        }
      }))
    })
  })

  # Dynamically filter data using explicit DuckDB SQL
  lapply(tables, function(tbl) {
    output[[paste0("table_", tbl)]] <- renderReactable({
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

      df <- dbGetQuery(con, sql, params = params)
      reactable(df, searchable = TRUE, pagination = TRUE)
    })
  })

}

shinyApp(ui, server)
