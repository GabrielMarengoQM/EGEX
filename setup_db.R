library(duckdb)
library(arrow)

db_file <- "mydb.duckdb"
wal_file <- paste0(db_file, ".wal")

# Remove the main database file if it exists
if (file.exists(db_file)) {
  file.remove(db_file)
}

# Remove the WAL file if it exists
if (file.exists(wal_file)) {
  file.remove(wal_file)
}

con <- dbConnect(duckdb(), db_file)

# File names to read in working directory
table_names <- c("genes", "impc_viability", "impc_phenotypes", "impc_wol", "omim_phenotypes_lethality", "omim_lethal_genes", "constraint_metrics")

# Read files & write to DuckDB
for (tbl in table_names) {
  df <- as.data.frame(read_parquet(paste0("./parquet_files/", tbl, ".parquet")))
  print(head(df))
  dbWriteTable(con, tbl, df, overwrite = TRUE)
}

dbDisconnect(con)
