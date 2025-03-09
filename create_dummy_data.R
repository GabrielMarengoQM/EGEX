library(arrow)
set.seed(123)

num_genes <- 20000
gene_ids <- sprintf("G%05d", 1:num_genes)

# Genes (one-to-one)
genes <- data.frame(
  GeneID = gene_ids,
  GeneSymbol = paste0("Gene", 1:num_genes),
  Chromosome = sample(c(1:22, "X", "Y"), num_genes, replace = TRUE),
  Position = sample(1e6:1e8, num_genes)
)

# Phenotypes (one-to-many)
phenotypes <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    Phenotype = sample(paste("Phenotype", LETTERS), sample(1:4, 1)),
    Severity = sample(1:10, 1),
    Prevalence = runif(1)
  )
}))

# Constraints (one-to-many)
constraints <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    ConstraintMetric = sample(c("High", "Moderate", "Low"), sample(1:3, 1)),
    Score = runif(1),
    Confidence = sample(c("High", "Medium", "Low"), 1)
  )
}))

# Expression (one-to-one)
expression <- data.frame(
  GeneID = gene_ids,
  Tissue = sample(c("Brain", "Heart", "Liver", "Kidney"), num_genes, replace = TRUE),
  TPM = runif(num_genes, 0, 100)
)

# Pathways (one-to-many)
pathways <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    Pathway = sample(paste("Pathway", LETTERS), sample(1:3, 1)),
    Importance = runif(1, 0, 1)
  )
}))

# Variants (one-to-many)
variants <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    VariantType = sample(c("Missense", "Nonsense", "Synonymous"), sample(1:2, 1)),
    Frequency = runif(1, 0, 0.05)
  )
}))

# Publications (one-to-many)
publications <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    PMID = sample(10000000:99999999, sample(1:5, 1)),
    Year = sample(1990:2025, 1)
  )
}))

# Protein Interactions (one-to-many)
protein_interactions <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    InteractionPartner = paste0("Gene", sample(1:num_genes, sample(1:3, 1))),
    InteractionStrength = runif(1)
  )
}))

# Disease Associations (one-to-many)
disease_associations <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    Disease = sample(paste("Disease", LETTERS), sample(1:2, 1)),
    AssociationScore = runif(1)
  )
}))

# Functional Annotations (one-to-many)
functional_annotations <- do.call(rbind, lapply(gene_ids, function(id) {
  data.frame(
    GeneID = id,
    Annotation = sample(c("GO:0008150", "GO:0003674", "GO:0005575"), sample(1:3, 1)),
    Evidence = sample(c("EXP", "IDA", "IEA"), 1)
  )
}))

# Write all tables to Parquet files
tables <- list(
  genes = genes,
  phenotypes = phenotypes,
  constraints = constraints,
  expression = expression,
  pathways = pathways,
  variants = variants,
  publications = publications,
  protein_interactions = protein_interactions,
  disease_associations = disease_associations,
  functional_annotations = functional_annotations
)

lapply(names(tables), function(tbl) {
  write_parquet(tables[[tbl]], paste0(tbl, ".parquet"))
})
