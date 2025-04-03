# reactomeORA_module.R Helpers
ReactomeEnrichResult <- function(gene, organism, pvalueCutoff, qvalueCutoff) {

  # Enrichment
  enrich_res <- enrichPathway(gene, #= genes,
                              organism, #= "human",
                              pvalueCutoff, #= input$pvalueCutoff,
                              qvalueCutoff, #= input$qvalueCutoff,
                              readable = TRUE)

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
