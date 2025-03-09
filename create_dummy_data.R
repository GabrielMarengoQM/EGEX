library(arrow)

set.seed(123)
n_genes <- 200

# Genes DataFrame
genes_df <- data.frame(
  GeneID = sprintf("G%05d", 1:n_genes),
  GeneSymbol = paste0("Gene", 1:n_genes),
  stringsAsFactors = FALSE
)
# Introduce NAs in ~5% of GeneSymbol
na_idx <- sample(1:n_genes, size = round(n_genes * 0.05))
genes_df$GeneSymbol[na_idx] <- NA
write_parquet(genes_df, "genes.parquet")

# Metrics DataFrame: each gene gets 1-3 rows
metrics_list <- lapply(genes_df$GeneID, function(g) {
  n <- sample(1:3, 1)
  data.frame(
    GeneID = rep(g, n),
    Essentiality_Metric = sample(c("Metric_A", "Metric_B", "Metric_C"), n, replace = TRUE),
    Score = round(runif(n, 0, 1), 2),
    stringsAsFactors = FALSE
  )
})
metrics_df <- do.call(rbind, metrics_list)
# Introduce NAs in ~10% of rows for Essentiality_Metric and Score
na_rows <- sample(1:nrow(metrics_df), size = round(0.1 * nrow(metrics_df)))
metrics_df$Essentiality_Metric[na_rows] <- NA
na_rows_score <- sample(1:nrow(metrics_df), size = round(0.1 * nrow(metrics_df)))
metrics_df$Score[na_rows_score] <- NA
write_parquet(metrics_df, "metrics.parquet")

# Pathways DataFrame: each gene gets 1-2 rows
pathways_list <- lapply(genes_df$GeneID, function(g) {
  n <- sample(1:2, 1)
  data.frame(
    GeneID = rep(g, n),
    Pathway = sample(c("Pathway_X", "Pathway_Y", "Pathway_Z", "Pathway_W"), n, replace = TRUE),
    Importance = sample(1:20, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
})
pathways_df <- do.call(rbind, pathways_list)
# Introduce NAs in ~10% of rows for Pathway and Importance
na_rows_path <- sample(1:nrow(pathways_df), size = round(0.1 * nrow(pathways_df)))
pathways_df$Pathway[na_rows_path] <- NA
na_rows_imp <- sample(1:nrow(pathways_df), size = round(0.1 * nrow(pathways_df)))
pathways_df$Importance[na_rows_imp] <- NA
write_parquet(pathways_df, "pathways.parquet")

cat("Parquet files generated: genes.parquet, metrics.parquet, pathways.parquet\n")
