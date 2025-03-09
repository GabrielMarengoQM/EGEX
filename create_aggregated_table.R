library(duckdb)
library(DBI)

# Connect to DuckDB (make sure your database is already populated with tables)
con <- dbConnect(duckdb(), "mydb.duckdb", read_only = FALSE)

# Get list of all tables, excluding the "aggregated" table if it already exists.
all_tables <- dbListTables(con)
tables_to_agg <- setdiff(all_tables, "aggregated")

# Function to build an aggregated subquery for one table.
build_subquery <- function(tbl) {
  fields <- dbListFields(con, tbl)
  # We assume every table has GeneID as the common key.
  non_key <- setdiff(fields, "GeneID")
  if (length(non_key) == 0) {
    # If there are no non-key columns, just select GeneID.
    agg_expr <- ""
  } else {
    # For each non-key column, build an aggregation expression.
    agg_expr <- paste(sapply(non_key, function(col) {
      # Use DISTINCT values, comma separated.
      sprintf("LISTAGG(DISTINCT %s, ', ') AS %s", col, col)
    }), collapse = ", ")
  }
  # Build the subquery.
  # If there are aggregated expressions, include them; otherwise, just GeneID.
  if (nzchar(agg_expr)) {
    qry <- sprintf("(SELECT GeneID, %s FROM %s GROUP BY GeneID) AS %s_agg", agg_expr, tbl, tbl)
  } else {
    qry <- sprintf("(SELECT GeneID FROM %s GROUP BY GeneID) AS %s_agg", tbl, tbl)
  }
  return(qry)
}

# Build a list of subqueries.
subqueries <- lapply(tables_to_agg, build_subquery)
names(subqueries) <- tables_to_agg

# Choose a primary table for the FROM clause.
# If a table named "genes" exists, use it; otherwise, use the first table.
primary_tbl <- if ("genes" %in% tables_to_agg) "genes" else tables_to_agg[1]

# The primary subquery:
primary_subquery <- subqueries[[primary_tbl]]
primary_alias <- paste0(primary_tbl, "_agg")

# Build the final SELECT query.
# Start with selecting GeneID from the primary subquery.
final_query <- sprintf("SELECT %s.GeneID", primary_alias)

# For each table (subquery), add all columns from that aggregated subquery.
for (tbl in tables_to_agg) {
  alias <- paste0(tbl, "_agg")
  # Get list of columns for this table (excluding GeneID).
  cols <- setdiff(dbListFields(con, tbl), "GeneID")
  if (length(cols) > 0) {
    for (col in cols) {
      final_query <- paste(final_query, sprintf(", %s.%s", alias, col))
    }
  }
}

# Start FROM clause with primary subquery.
final_query <- paste(final_query, "FROM", primary_subquery)

# For each remaining table, left join on GeneID.
for (tbl in setdiff(tables_to_agg, primary_tbl)) {
  alias <- paste0(tbl, "_agg")
  subq <- subqueries[[tbl]]
  final_query <- paste(final_query, sprintf("LEFT JOIN %s ON %s.GeneID = %s.GeneID", subq, primary_alias, alias))
}

# Optional: Print the final query for debugging
cat("Final aggregated query:\n", final_query, "\n")

# Drop existing aggregated table if exists and create the new aggregated table.
dbExecute(con, "DROP TABLE IF EXISTS aggregated")
dbExecute(con, paste("CREATE TABLE aggregated AS", final_query))

dbDisconnect(con)
