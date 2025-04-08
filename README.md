# Gene Essentiality Database Explorer

**How to Run:**  
Download the complete **EGE** folder to your computer. Then open R or RStudio and run the following command:
`shiny::runApp()`

**Functionality:**  
- **Filtering Database:**  
  Dynamically filter through a comprehensive DuckDB-based gene database using automatically generated user interface elements.
- **Interactive Plotting:**  
  Explore gene annotations and analysis results with interactive visualizations using Plotly and Reactable.
- **Over-representation Analysis:**  
  Run enrichment analyses (e.g., odds ratio calculations) on user-selected gene sets to test for enrichment of particular features.

**Data Types:**

- **Organism Data:**
  - IMPC mouse KOs
  - IMPC Embyonic lethality windows (Windows of Lethality)
  - Model organism Mouse orthologs
  - OMIM disease genes
  - OMIM lethal genes

- **Constraint Metrics:**
  - DepMap
  - gnomAD
  - AlphaMissense
  - Assays from Huangfu et al, Mair et al

- **Protein–Protein Interactions:**
  - Data from STRING

- **Gene Ontology:**  
  GO annotations providing functional insights

- **Reactome:**  
  Pathway annotations

- **GTEx:**  
  Tissue expression data

- **FUSIL:**  
  Functional gene intolerance scores

- **Paralogs:**  
  Information about duplicated gene copies

- **Human Phenotype Ontology:**  
  Gene-to-phenotype associations
