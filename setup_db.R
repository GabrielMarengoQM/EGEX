library(duckdb)
library(arrow)

con <- dbConnect(duckdb(), "mydb.duckdb")

# File names to read in working dir ----
table_names <- c(
  "genes", "phenotypes", "constraints", "expression",
  "pathways", "variants", "publications", "protein_interactions",
  "disease_associations", "functional_annotations"
)
# table_names <- c(
#   "genes", "phenotypes", "constraints"
# )

# read files & write to db
for (tbl in table_names) {
  df <- as.data.frame(read_parquet(paste0(tbl, ".parquet"))) # read in Parquet table
  dbWriteTable(con, tbl, df, overwrite = TRUE) # write to duckDB
}

dbDisconnect(con)
