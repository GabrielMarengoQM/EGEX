library(duckdb)
library(DBI)
library(arrow)

# Connect or create DuckDB file
con <- dbConnect(duckdb(), dbdir = "mydb.duckdb")

# Load parquet files
genes_df <- read_parquet("genes.parquet")
metrics_df <- read_parquet("metrics.parquet")
pathways_df <- read_parquet("pathways.parquet")

# Write tables (overwrite if updating)
dbWriteTable(con, "genes", genes_df, overwrite = TRUE)
dbWriteTable(con, "metrics", metrics_df, overwrite = TRUE)
dbWriteTable(con, "pathways", pathways_df, overwrite = TRUE)

dbDisconnect(con)
