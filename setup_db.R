library(duckdb)
library(arrow)

con <- dbConnect(duckdb(), "mydb.duckdb")

table_names <- c(
  "genes", "phenotypes", "constraints", "expression",
  "pathways", "variants", "publications", "protein_interactions",
  "disease_associations", "functional_annotations"
)

for (tbl in table_names) {
  df <- as.data.frame(read_parquet(paste0(tbl, ".parquet")))
  dbWriteTable(con, tbl, df, overwrite = TRUE)
}

dbDisconnect(con)
