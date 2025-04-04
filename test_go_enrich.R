# NO ENRICHED TERMS
genes_test <- c(
  "2709", "5507", "133874", "1466", "284274", "127281", "54940", "613211", "805", "343450",
  "1670", "7159", "23268", "1365", "245913", "56265", "84541", "200424", "162083", "85364",
  "55507", "400831", "8111", "5994", "10233", "49861", "3039", "4478", "221191", "25850",
  "22998", "84970", "54929", "641372", "25798"
)

# NO ENRICHED TERMS
genes_test <- c("7280", "57786", "6427", "23469", "23327", "10268", "7278")

# ENIRCHED TERMS
genes_test <- c("4312", "8318", "10874", "55143", "55388", "991") 

# INVALID LIST
genes_test <- c()


enrich_res_test <- enrichGO(
  gene          = genes_test,
  OrgDb         = org.Hs.eg.db,
  ont           = "BP",
  pvalueCutoff  = 0.05,
  qvalueCutoff  = 0.2
)

enrich_res_test <- enrichPathway(gene = genes_test,
                            organism = "human",
                            pvalueCutoff = 0.000005,
                            qvalueCutoff = 0.2)

# Checks if no enirched terms
df <- as.data.frame(enrich_res_test)
if(nrow(df) == 0) {
  print(data.frame(Message = "No enriched terms found"))
}

# Checks if invalid input
if (is.null(enrich_res_test)) {
  print('invalid input')
}