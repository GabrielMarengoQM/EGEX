```markdown
## Gene Essentiality Database Explorer

A lightweight Shiny app for browsing and filtering up-to-date gene essentiality annotations stored in DuckDB.

### Key Features

- **Live DuckDB backend**: Queries a local DuckDB database for fast, on-the-fly data access.  
- **Dynamic filters**: UI inputs are generated automatically from the database schema.  
- **Interactive tables & plots**: Built with Reactable and Plotly for sorting, paging, zooming, etc.  
- **Exportable data**: Download your filtered view as a CSV for downstream analyses.

### Quickstart

1. **Clone the repo**  
   ```bash
   git clone https://github.com/GabrielMarengoQM/EGEX.git
   cd EGEX
   ```
2. **Install R dependencies**  
   In R or RStudio, run:
   ```r
   install.packages(c("shiny", "duckdb", "arrow", "reactable", "plotly"))
   ```
3. **Prepare the database**  
   ```bash
   Rscript create_dummy_data.R
   Rscript setup_db.R
   ```
4. **Launch the app**  
   ```bash
   Rscript -e "shiny::runApp('app.R', port = 3838)"
   ```
   Then open <http://localhost:3838> in your browser.

### License

MIT — see [LICENSE](LICENSE)

### Questions?

Contact Gabriel Marengo at <your.email@example.com>.
```

