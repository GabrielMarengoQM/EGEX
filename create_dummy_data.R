library(arrow)
set.seed(123)

num_genes <- 20000
gene_ids <- sprintf("G%05d", 1:num_genes)

# ## WITH NA ----
# # ---------------------------
# # Genes Table (one-to-one)
# # ---------------------------
# genes <- data.frame(
#   GeneID = gene_ids,
#   GeneSymbol = paste0("Gene", 1:num_genes),
#   Chromosome = sample(c(1:22, "X", "Y"), num_genes, replace = TRUE),
#   Position = sample(1e6:1e8, num_genes)
# )
# # Introduce ~50% missingness in GeneSymbol
# na_idx <- sample(1:num_genes, size = round(0.5 * num_genes))
# genes$GeneSymbol[na_idx] <- NA
#
# # ---------------------------
# # Phenotypes Table (one-to-many)
# # ---------------------------
# phenotypes <- do.call(rbind, lapply(gene_ids, function(id) {
#   n <- sample(1:4, 1)
#   df <- data.frame(
#     GeneID = id,
#     Phenotype = sample(paste("Phenotype", LETTERS), n, replace = TRUE),
#     Severity = sample(1:10, n, replace = TRUE),
#     Prevalence = runif(n)
#   )
#   # Introduce ~50% missingness in Phenotype and Severity
#   if(n > 0){
#     na_idx <- sample(1:n, size = round(0.5 * n))
#     df$Phenotype[na_idx] <- NA
#     na_idx2 <- sample(1:n, size = round(0.5 * n))
#     df$Severity[na_idx2] <- NA
#   }
#   df
# }))

# # ---------------------------
# # Constraints Table (one-to-many)
# # ---------------------------
# constraints <- do.call(rbind, lapply(gene_ids, function(id) {
#   n <- sample(1:3, 1)
#   df <- data.frame(
#     GeneID = id,
#     ConstraintMetric = sample(c("High", "Moderate", "Low"), n, replace = TRUE),
#     Score = runif(n),
#     Confidence = sample(c("High", "Medium", "Low"), n, replace = TRUE)
#   )
#   # Introduce ~50% missingness in ConstraintMetric and Score
#   if(n > 0){
#     na_idx <- sample(1:n, size = round(0.5 * n))
#     df$ConstraintMetric[na_idx] <- NA
#     na_idx2 <- sample(1:n, size = round(0.5 * n))
#     df$Score[na_idx2] <- NA
#   }
#   df
# }))

## WITH "NA" ----
genes <- data.frame(
  GeneID = gene_ids,
  GeneSymbol = paste0("Gene", 1:num_genes),
  Chromosome = sample(c(1:22, "X", "Y"), num_genes, replace = TRUE),
  Position = sample(1e6:1e8, num_genes)
)
# Introduce ~50% missingness in GeneSymbol
na_idx <- sample(1:num_genes, size = round(0.5 * num_genes))
genes$GeneSymbol[na_idx] <- "NA"

# ---------------------------
# Phenotypes Table (one-to-many)
# ---------------------------
phenotypes <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:4, 1)
  df <- data.frame(
    GeneID = id,
    Phenotype = sample(paste("Phenotype", LETTERS), n, replace = TRUE),
    Severity = sample(1:10, n, replace = TRUE),
    Prevalence = runif(n)
  )
  # Introduce ~50% missingness in Phenotype and Severity
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$Phenotype[na_idx] <- "NA"
    na_idx2 <- sample(1:n, size = round(0.5 * n))
    df$Severity[na_idx2] <- "NA"
  }
  df
}))

# ---------------------------
# Constraints Table (one-to-many)
# ---------------------------
constraints <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:3, 1)
  df <- data.frame(
    GeneID = id,
    ConstraintMetric = sample(c("High", "Moderate", "Low"), n, replace = TRUE),
    Score = runif(n),
    Confidence = sample(c("High", "Medium", "Low"), n, replace = TRUE)
  )
  # Introduce ~50% missingness in ConstraintMetric and Score
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$ConstraintMetric[na_idx] <- "NA"
    na_idx2 <- sample(1:n, size = round(0.5 * n))
    df$Score[na_idx2] <- "NA"
  }
  df
}))

# ---------------------------
# Expression Table (one-to-one)
# ---------------------------
expression <- data.frame(
  GeneID = gene_ids,
  Tissue = sample(c("Brain", "Heart", "Liver", "Kidney"), num_genes, replace = TRUE),
  TPM = runif(num_genes, 0, 100)
)
# Introduce ~50% missingness in Tissue
na_idx <- sample(1:num_genes, size = round(0.5 * num_genes))
expression$Tissue[na_idx] <- "NA"

# ---------------------------
# Pathways Table (one-to-many)
# ---------------------------
pathways <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:3, 1)
  df <- data.frame(
    GeneID = id,
    Pathway = sample(paste("Pathway", LETTERS), n, replace = TRUE),
    Importance = runif(n, 0, 1)
  )
  # Introduce ~50% missingness in Pathway
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$Pathway[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Variants Table (one-to-many)
# ---------------------------
variants <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:2, 1)
  df <- data.frame(
    GeneID = id,
    VariantType = sample(c("Missense", "Nonsense", "Synonymous"), n, replace = TRUE),
    Frequency = runif(n, 0, 0.05)
  )
  # Introduce ~50% missingness in VariantType
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$VariantType[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Publications Table (one-to-many)
# ---------------------------
publications <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:5, 1)
  df <- data.frame(
    GeneID = id,
    PMID = sample(10000000:99999999, n, replace = TRUE),
    Year = sample(1990:2025, n, replace = TRUE)
  )
  # Introduce ~50% missingness in Year
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$Year[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Protein Interactions Table (one-to-many)
# ---------------------------
protein_interactions <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:3, 1)
  df <- data.frame(
    GeneID = id,
    InteractionPartner = paste0("Gene", sample(1:num_genes, n, replace = TRUE)),
    InteractionStrength = runif(n)
  )
  # Introduce ~50% missingness in InteractionPartner
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$InteractionPartner[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Disease Associations Table (one-to-many)
# ---------------------------
disease_associations <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:2, 1)
  df <- data.frame(
    GeneID = id,
    Disease = sample(paste("Disease", LETTERS), n, replace = TRUE),
    AssociationScore = runif(n)
  )
  # Introduce ~50% missingness in Disease
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$Disease[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Functional Annotations Table (one-to-many)
# ---------------------------
functional_annotations <- do.call(rbind, lapply(gene_ids, function(id) {
  n <- sample(1:3, 1)
  df <- data.frame(
    GeneID = id,
    Annotation = sample(c("GO:0008150", "GO:0003674", "GO:0005575"), n, replace = TRUE),
    Evidence = sample(c("EXP", "IDA", "IEA"), n, replace = TRUE)
  )
  # Introduce ~50% missingness in Annotation
  if(n > 0){
    na_idx <- sample(1:n, size = round(0.5 * n))
    df$Annotation[na_idx] <- "NA"
  }
  df
}))

# ---------------------------
# Write all tables to Parquet files
# ---------------------------
# tables <- list(
#   genes = genes,
#   phenotypes = phenotypes,
#   constraints = constraints,
#   expression = expression,
#   pathways = pathways,
#   variants = variants,
#   publications = publications,
#   protein_interactions = protein_interactions,
#   disease_associations = disease_associations,
#   functional_annotations = functional_annotations
# )
tables <- list(
  genes = genes,
  phenotypes = phenotypes,
  constraints = constraints
  # expression = expression,
  # pathways = pathways,
  # variants = variants,
  # publications = publications,
  # protein_interactions = protein_interactions,
  # disease_associations = disease_associations,
  # functional_annotations = functional_annotations
)

lapply(names(tables), function(tbl) {
  write_parquet(tables[[tbl]], paste0(tbl, ".parquet"))
})
