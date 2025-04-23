library(duckdb)
library(DBI)

# Connect to the DuckDB database (adjust path if needed)
con <- dbConnect(duckdb(), "mydb.duckdb", read_only = FALSE)

# Get all tables and exclude the "aggregated" table if it exists
all_tables <- dbListTables(con)
tables_to_agg <- setdiff(all_tables, "aggregated")

if(length(tables_to_agg) == 0) {
  stop("No tables available to aggregate!")
}

# Function to build an aggregated subquery for a given table.
# For each non-key column, we aggregate distinct values using LISTAGG.
build_subquery <- function(tbl) {
  fields <- dbListFields(con, tbl)
  non_key <- setdiff(fields, "GeneID")
  if (length(non_key) == 0) {
    qry <- sprintf("(SELECT GeneID FROM %s GROUP BY GeneID) AS %s_agg", tbl, tbl)
  } else {
    agg_expr <- paste(sapply(non_key, function(col) {
      sprintf("LISTAGG(DISTINCT %s, ', ') AS %s", col, col)
    }), collapse = ", ")
    qry <- sprintf("(SELECT GeneID, %s FROM %s GROUP BY GeneID) AS %s_agg", agg_expr, tbl, tbl)
  }
  return(qry)
}

# Build aggregated subqueries for every table in the current DB.
subqueries <- lapply(tables_to_agg, build_subquery)
names(subqueries) <- tables_to_agg

# Choose a primary table for the FROM clause.
# If a table named "genes" exists, use it; otherwise, use the first table.
primary_tbl <- if ("genes" %in% tables_to_agg) "genes" else tables_to_agg[1]
primary_alias <- paste0(primary_tbl, "_agg")
primary_subquery <- subqueries[[primary_tbl]]

# Start building the final SELECT query with the primary table's GeneID.
final_query <- sprintf("SELECT %s.GeneID", primary_alias)

# For each table, add its aggregated (non-key) columns.
for (tbl in tables_to_agg) {
  alias <- paste0(tbl, "_agg")
  cols <- setdiff(dbListFields(con, tbl), "GeneID")
  if (length(cols) > 0) {
    for (col in cols) {
      final_query <- paste(final_query, sprintf(", %s.%s", alias, col))
    }
  }
}

# Start the FROM clause with the primary subquery.
final_query <- paste(final_query, "FROM", primary_subquery)

# LEFT JOIN each of the remaining tables on GeneID.
for (tbl in setdiff(tables_to_agg, primary_tbl)) {
  alias <- paste0(tbl, "_agg")
  subq <- subqueries[[tbl]]
  final_query <- paste(final_query, sprintf("LEFT JOIN %s ON %s.GeneID = %s.GeneID", subq, primary_alias, alias))
}

# Optional: Print the final query for debugging.
cat("Final aggregated query:\n", final_query, "\n")

# Drop the existing aggregated table if it exists, then create the new one.
dbExecute(con, "DROP TABLE IF EXISTS aggregated")
dbExecute(con, paste("CREATE TABLE aggregated AS", final_query))

# (Optional) View the aggregated table as a data frame
aggregated_df <- dbGetQuery(con, "SELECT * FROM aggregated")
print(head(aggregated_df))

dbDisconnect(con)
