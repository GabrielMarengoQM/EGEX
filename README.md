# Gene Essentiality Database Explorer

This Shiny app provides a continuously updated database for gene-level annotations associated with gene essentiality. It is designed to facilitate the exploration, filtering, and analysis of genes using real-time data from a DuckDB database.

## Features

- **Dynamic Database Integration:**  
  The app leverages a DuckDB database to store and query gene annotations and related data. It dynamically adapts to changes in the underlying database, making it easy to update with new data.

- **Comprehensive Gene Annotations:**  
  The database includes gene-level annotations such as:
  - Gene identifiers and symbols
  - Phenotype associations
  - Constraint metrics
  - Expression levels
  - Pathway involvement
  - Variant details
  - Publications, protein interactions, disease associations, and functional annotations

- **Flexible Filtering:**  
  Dynamically generated UI components allow users to filter data by various columns (e.g., Gene Symbol, Essentiality Metric, Score, etc.) without any hard-coded inputs. This is achieved using functional programming techniques that automatically generate filters based on the database schema.

- **Interactive Visualization:**  
  The app utilizes Reactable for interactive table displays and Plotly for dynamic plotting. Users can interact with tables and plots to gain insights into gene essentiality patterns and related annotations.

- **Data Export:**  
  Users have the option to download filtered datasets for further analysis.

## Getting Started

1. **Data Setup:**  
   - Run `create_dummy_data.R` to generate the initial dummy data.
   - Run `setup_db.R` to populate the DuckDB database with the generated data.

2. **Run the App:**  
   Launch the Shiny app by running `app.R` in RStudio.

3. **GitHub Integration:**  
   The project is fully integrated with Git for version control. You can push updates directly from RStudio or the terminal.

## Purpose

This app is intended to serve as a powerful tool for researchers and analysts who need to explore and analyze gene-level annotations associated with gene essentiality. By providing a robust, continuously updated database with flexible filtering, plotting, and analysis options, the app supports rapid insights into the genetic factors underpinning essentiality.

## Technologies

- **R and Shiny:** For the interactive web application.
- **DuckDB:** As the backend database for efficient querying and data storage.
- **Arrow:** For reading and writing Parquet files.
- **Reactable & Plotly:** For interactive tables and visualizations.
- **Functional Programming (lapply, map, etc.):** For dynamic generation of UI and server logic.

## License

This project is open-source and available under the [MIT License](LICENSE).

## Contact

For any questions or feedback, please reach out to [Your Name](mailto:your.email@example.com).
