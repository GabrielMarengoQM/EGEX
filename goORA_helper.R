# goORA_module.R Helpers
GoEnrichResult <- function(gene, OrgDb, ont, pvalueCutoff, qvalueCutoff) {

  enrich_res <- enrichGO(gene, OrgDb, ont, pvalueCutoff, qvalueCutoff)

  # Validate for results table
  validate(
    need(!is.null(enrich_res), "Error: Analysis did not return any results.")
  )
  df <- as.data.frame(enrich_res)
  validate(
    need(nrow(df) > 0, "Error: No significant Reactome pathways found.")
  )
  df

  # Validate for Plot
  validate(
    need(!is.null(enrich_res), "Error: Analysis did not return any results.")
  )
  df <- as.data.frame(enrich_res)
  validate(
    need(nrow(df) > 0, "Error: No significant Reactome pathways found.")
  )
  plot <- dotplot(enrich_res, showCategory = 10)

  return(list(plot, df))
}

