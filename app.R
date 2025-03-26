library(shiny)
library(bslib) # additional UI
library(duckdb) # duckDB DB
library(DBI) # DB connection
library(DT) # tables
library(plotly) # plots
library(dplyr) # piping and funcs
library(arrow) # parquet files
library(zip) # downloads
library(UpSetR) # upset plot
library(shinyWidgets) # additional UI
library(shinycssloaders) # loaders

# Connect to DuckDB
con <- dbConnect(duckdb(), "mydb.duckdb")
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "aggregated")
saved_gene_lists <- reactiveValues(data = list())

# A reactiveVal to store the name of the currently expanded gene list
currentExpandedGeneList <- reactiveVal(NULL)

ui <- page_navbar(
  title = "EGEx",
  padding = "0.4rem",
  theme = bs_theme(bootswatch = "cosmo"),

  # Include custom CSS styles in the document head
  tags$head(
    tags$style(HTML("
      /* ================= Custom Sidebar CSS ================= */

      /* Style the toggle button within the custom sidebar layout */
      .custom-sidebar .bslib-sidebar-layout > .collapse-toggle {
        padding: 15px 10px;              /* Add vertical and horizontal padding */
        background-color: #007398;       /* Set the background color of the button */
        position: absolute;              /* Position the button absolutely within its container */
        border-radius: 0 5px 5px 0;        /* Round the right corners of the button */
        writing-mode: vertical-rl;       /* Display text vertically from right to left */
        text-orientation: mixed;         /* Allow mixed text orientation */
        height: auto;                    /* Let the button adjust its height automatically */
        z-index: 1000;                   /* Place the button on top of other elements */
        display: flex;                   /* Use flex layout for inner content alignment */
        align-items: center;             /* Vertically center the content */
        justify-content: center;         /* Horizontally center the content */
        transition: all 0.3s;            /* Smoothly animate property changes */
        top: 20px;                       /* Position the button 50px from the top */
        right: -40px;                   /* Position the button 40px outside the right edge */
      }

      /* Adjust the button's position when the sidebar is closed */
      .custom-sidebar .bslib-sidebar-layout:not(.sidebar-open) > .collapse-toggle {
        left: 0;                        /* Align the button to the left edge */
        right: auto;                    /* Reset the right positioning */
      }

      /* Style the icon inside the toggle button */
      .custom-sidebar .bslib-sidebar-layout > .collapse-toggle > .collapse-icon {
        fill: white !important;         /* Force the icon color to white */
        margin-bottom: 10px;             /* Add margin below the icon */
      }

      /* Add static text to the toggle button using the ::after pseudo-element */
      .custom-sidebar .bslib-sidebar-layout > .collapse-toggle::after {
        color: white;                   /* Set the text color to white */
        font-size: 1.1em;               /* Slightly enlarge the font size */
        text-transform: uppercase;      /* Transform text to uppercase */
        letter-spacing: 1px;            /* Add spacing between letters */
        margin-top: 10px;               /* Add margin above the text */
      }

      /* When the toggle button is expanded (sidebar open), display 'Close Sidebar' */
      .custom-sidebar .bslib-sidebar-layout > .collapse-toggle[aria-expanded='true']::after {
        content: 'Close Sidebar';
      }

      /* When the toggle button is collapsed (sidebar closed), display 'Sidebar' */
      .custom-sidebar .bslib-sidebar-layout > .collapse-toggle[aria-expanded='false']::after {
        content: 'Sidebar';
      }

      /* Adjust the main content area when the sidebar is open */
      .custom-sidebar .bslib-sidebar-layout > .main {
        margin-left: 300px;             /* Leave a left margin equal to the sidebar width */
        transition: margin-left 0.3s;    /* Animate margin changes smoothly */
        width: calc(100% - 300px);        /* Adjust the width to account for the sidebar */
      }

      /* When the sidebar is closed, let the main content take full width */
      .custom-sidebar .bslib-sidebar-layout:not(.sidebar-open) > .main {
        margin-left: 0;                 /* Remove the left margin */
        width: 100%;                    /* Set full width */
      }

      /* Adjust the margin within the sidebar itself */
      .custom-sidebar .bslib-sidebar-layout > .sidebar {
        margin-left: 20px;              /* Add a left margin inside the sidebar */
        padding-right: 0px;             /* Add desired right padding (adjust as needed) */
      }

      /* Add right padding to the container-fluid with these classes */
      .container-fluid.html-fill-item.html-fill-container {
        padding-right: 20px;            /* Add 20px of right padding */
      }
        /* Reduce extra padding to the sidebar container */
     .custom-sidebar .sidebar-content.bslib-gap-spacing {
      padding-top: 10px !important;
      }
      /* ================= End Custom Sidebar CSS ================= */
      /* Add extra 32px padding to the main container */
    .main.bslib-gap-spacing.html-fill-container {
      padding-left: 32px;
    }

    "))
  ),

  # ---------------- "Explore Data" Panel (with custom sidebar) ----------------
    nav_panel(
      "Explore Data",
      div(
        class = "custom-sidebar",
      page_sidebar(
        sidebar = sidebar(
          width = "500px",             # Set the sidebar width
          open = c("open"),            # Initially open the sidebar
          div(
            class = "overflow-auto",
            style = "max-height: 85vh;",  # Allow the sidebar content to scroll if too tall
            accordion(
              # Accordion Panel: Saved Gene Lists
              accordion_panel(
                tagList(icon("list"), "Saved Gene Lists"),
                tagList(
                  actionButton("add_gene_list", "+ Save current gene list", class = "btn btn-success"),
                  uiOutput("saved_gene_lists_ui")
                ),
                value = "saved"
              ),
              # Accordion Panel: Table/Plot Options with nested accordion
              accordion_panel(
                tagList(icon("columns"), "Table/Plot Options"),
                tagList(
                  accordion(
                    # Nested Accordion: Table Options Panel
                    accordion_panel(
                      tagList(icon("table"), "Table Options"),
                      tagList(
                        selectInput(
                          "agg_table",
                          "Table to display:",
                          choices = dbListTables(con),  # or use 'all_tables' if already defined
                          selected = "aggregated"
                        )
                      ),
                      value = "table_options"
                    ),
                    # Nested Accordion: Plot Options Panel
                    accordion_panel(
                      tagList(icon("sliders"), "Plot Options"),
                      tagList(
                        selectInput(
                          "plot_type",
                          "Select Plot Type:",
                          choices = c(
                            "Bar Chart/Histogram",
                            "Violin/Box Plot",
                            "Scatter Plot",
                            "Stacked Bar Chart",
                            "UpSet Plot"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.plot_type == 'Bar Chart/Histogram'",
                          selectInput(
                            "bar_table",
                            "Select table for Bar Chart/Histogram:",
                            choices = individual_tables,
                            selected = individual_tables[1]
                          ),
                          selectInput("bar_col", "Select column:", choices = NULL),
                          uiOutput("bar_bin_size_ui"),
                          checkboxInput("bar_show_na", "Plot missing values", value = FALSE),
                          radioButtons(
                            "bar_y_type",
                            "Y-axis type:",
                            choices = c("number of genes", "percentage of genes"),
                            selected = "number of genes",
                            inline = TRUE
                          ),
                          selectizeInput("bar_gene_lists", "Select Gene Lists:", choices = NULL, multiple = TRUE)
                        ),
                        conditionalPanel(
                          condition = "input.plot_type == 'Violin/Box Plot'",
                          uiOutput("violin_table_ui"),
                          selectInput("violin_col", "Select numeric column:", choices = NULL),
                          checkboxInput("violin_show_points", "Show all points", value = FALSE),
                          selectizeInput("violin_gene_lists", "Select Gene Lists:", choices = NULL, multiple = TRUE)
                        ),
                        conditionalPanel(
                          condition = "input.plot_type == 'Scatter Plot'",
                          selectInput(
                            "scatter_table_x",
                            "Select table for X:",
                            choices = individual_tables,
                            selected = individual_tables[1]
                          ),
                          selectInput("scatter_x", "Select X column:", choices = NULL),
                          selectInput(
                            "scatter_table_y",
                            "Select table for Y:",
                            choices = individual_tables,
                            selected = individual_tables[1]
                          ),
                          selectInput("scatter_y", "Select Y column:", choices = NULL),
                          selectizeInput(
                            "scatter_gene_lists",
                            "Select Gene Lists:",
                            choices = NULL,
                            selected = "Current List",
                            multiple = TRUE
                          )
                        ),
                        conditionalPanel(
                          condition = "input.plot_type == 'Stacked Bar Chart'",
                          selectInput(
                            "stack_table_x",
                            "Select table for X:",
                            choices = individual_tables,
                            selected = individual_tables[1]
                          ),
                          selectInput("stack_x", "Select X column:", choices = NULL),
                          uiOutput("stack_bin_size_ui"),
                          uiOutput("stack_table_y_ui"),
                          selectInput("stack_y", "Select Y column (factor):", choices = NULL),
                          radioButtons(
                            "stack_y_type",
                            "Y-axis display:",
                            choices = c("number of genes", "percentage of genes"),
                            selected = "percentage of genes",
                            inline = TRUE
                          ),
                          checkboxInput("stack_show_na_x", "Show missing values in X", value = FALSE),
                          checkboxInput("stack_show_na_y", "Show missing values in Y", value = FALSE),
                          selectizeInput(
                            "stack_gene_list",
                            "Select Gene List:",
                            choices = NULL,
                            selected = "Current List"
                          )
                        ),
                        conditionalPanel(
                          condition = "input.plot_type == 'UpSet Plot'",
                          selectizeInput(
                            "upset_gene_lists",
                            "Select Gene Lists:",
                            choices = NULL,
                            multiple = TRUE
                          )
                        )
                      ),
                      value = "plot_options"
                    ),
                    open = NULL  # Ensure that both nested panels are closed by default
                  )
                ),
                value = "options"
              ),
              # Accordion Panel: Filter Controls
              accordion_panel(
                tagList(icon("filter"), "Filter Controls"),
                tagList(
                  fluidRow(
                    column(6, actionButton("clear_filters", "Clear Filters", width = "100%")),
                    column(6, actionButton("show_filters", "View Filters", width = "100%"))
                  ),
                  hr(),
                  fluidRow(
                    column(12, actionButton("list_input", "Input custom list", width = "100%"))
                  ),
                  hr(),
                  # Dynamic filter inputs for each table (excluding GeneID)
                  lapply(individual_tables, function(tbl) {
                    tagList(
                      h5(tbl),
                      uiOutput(paste0("filters_", tbl)),
                      hr()
                    )
                  })
                ),
                value = "controls"
              ),
              open = "controls"  # Set the default open top-level accordion panel
            )
          )
        ),
        # Main content area with tabs
        tabsetPanel(
          tabPanel(
            "Gene table",
            withSpinner(card(
              DT::dataTableOutput("aggregated_table"),
              full_screen = TRUE, height = 625
            )),
            fluidRow(
              column(3, downloadButton("download_agg_table", "Download Table"))
            )
          ),
          tabPanel(
            "Plot",
            withSpinner(uiOutput("plot_ui")),
            withSpinner(DT::dataTableOutput("plot_data_table")),
            fluidRow(
              column(3, downloadButton("download_plot_df", "Download Plot Data"))
            )
          )
        )
      )
    )
  ),

  # ---------------- "Downloads" Panel (Not wrapped in custom-sidebar) ----------------
  nav_panel(
    "Downloads",
    fluidRow(
      column(6,
             h4("Download EGEx DataBase"),
             selectInput("db_file_type", "Select DB file type:",
                         choices = c("csv", "tsv", "parquet"), selected = "csv"),
             downloadButton("download_db", "Download DB")
      )
    ),
    fluidRow(
      column(6,
             h4("Download Gene Lists saved by User"),
             uiOutput("download_gene_lists_ui")
      )
    )
  )
)

server <- function(input, output, session) {

  ### Reactive gene mapping from the "genes" table (assumes columns GeneID and gene_symbol)
  gene_mapping <- reactive({
    dbReadTable(con, "genes")
  })

  ### SAVED GENE LISTS UI (each list now shows a single "Expand" button)
  output$saved_gene_lists_ui <- renderUI({
    if(length(saved_gene_lists$data) == 0) {
      HTML("<em>No saved gene lists.</em>")
    } else {
      tagList(
        lapply(names(saved_gene_lists$data), function(name) {
          count <- length(saved_gene_lists$data[[name]]$genes)
          expandId <- paste0("expand_", gsub(" ", "_", name))
          fluidRow(
            column(9, strong(name)),
            column(3, actionButton(expandId, "Expand", class = "btn-sm"))
          )
        })
      )
    }
  })

  # When a saved gene list's "Expand" button is clicked, show a modal with details.
  observe({
    req(saved_gene_lists$data)
    for(name in names(saved_gene_lists$data)) {
      local({
        expandId <- paste0("expand_", gsub(" ", "_", name))
        observeEvent(input[[expandId]], {
          currentExpandedGeneList(name)
          # Get saved GeneIDs and query the genes table for their gene symbols
          geneIDs <- saved_gene_lists$data[[name]]$genes
          count <- length(saved_gene_lists$data[[name]]$genes)
          symbols <- if(length(geneIDs) == 0) {
            "None"
          } else {
            query <- sprintf("SELECT symbol FROM genes WHERE GeneID IN (%s)",
                             paste(shQuote(geneIDs), collapse = ", "))
            res <- dbGetQuery(con, query)
            paste(res$symbol, collapse = ", ")
          }
          showModal(modalDialog(
            title = paste("Gene List:", name),
            tagList(
              tags$div(tags$strong("Number of Genes:"), count),
              tags$div(HTML(paste("<strong>Gene List:</strong><br>", symbols)))
            ),
            footer = tagList(
              actionButton("apply_modal", "Apply Filters"),
              actionButton("remove_modal", "Remove", class = "btn btn-danger")
            ),
            easyClose = TRUE
          ))
        }, ignoreInit = TRUE)
      })
    }
  })

  observeEvent(input$apply_modal, {
    req(currentExpandedGeneList())
    geneListName <- currentExpandedGeneList()
    saved_filters <- saved_gene_lists$data[[geneListName]]$filters
    for(key in names(saved_filters)) {
      val <- saved_filters[[key]]
      if (is.logical(val)) {
        updateCheckboxInput(session, key, value = val)
      } else if(is.numeric(val)) {
        updateSliderInput(session, key, value = val)
      } else {
        updateSelectizeInput(session, key, selected = val)
      }
    }
    removeModal()
  })

  observeEvent(input$remove_modal, {
    req(currentExpandedGeneList())
    geneListName <- currentExpandedGeneList()
    saved_gene_lists$data[[geneListName]] <- NULL
    removeModal()
  })

  ### CURRENT FILTERS UI (for modal)
  output$current_filters <- renderUI({
    all_inputs <- reactiveValuesToList(input)
    filter_keys <- names(all_inputs)[sapply(names(all_inputs), function(x) {
      any(sapply(individual_tables, function(tbl) {
        startsWith(x, paste0(tbl, "_"))
      }))
    })]
    filters <- all_inputs[filter_keys]
    tables <- unique(sapply(filter_keys, function(x) strsplit(x, "_")[[1]][1]))
    html_out <- ""
    for(tbl in tables) {
      tbl_keys <- filter_keys[startsWith(filter_keys, paste0(tbl, "_"))]
      rows <- sapply(tbl_keys, function(key) {
        colname <- sub(paste0("^", tbl, "_"), "", key)
        value <- filters[[key]]
        if (is.vector(value) && length(value) > 1) {
          value <- paste(value, collapse = ", ")
        }
        paste0("<tr><td style='padding:4px;'><strong>", colname, "</strong></td>",
               "<td style='padding:4px;'>", value, "</td></tr>")
      })
      tbl_html <- paste0("<h4>", tbl, "</h4>",
                         "<table style='width:100%; border-collapse: collapse; border: 1px solid #ccc;'>",
                         "<tr><th style='padding:4px;'>Filter</th><th style='padding:4px;'>Value</th></tr>",
                         paste(rows, collapse = ""), "</table>")
      html_out <- paste(html_out, tbl_html, sep = "<br>")
    }
    HTML(html_out)
  })

  ### DYNAMIC FILTER UI for each table
  lapply(individual_tables, function(tbl) {
    output[[paste0("filters_", tbl)]] <- renderUI({
      cols <- setdiff(dbListFields(con, tbl), "GeneID")
      ui_list <- lapply(cols, function(col) {
        input_id <- paste(tbl, col, sep = "_")
        na_id <- paste(input_id, "na", sep = "_")
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        if (is.numeric(sample_val)) {
          vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
          tagList(
            sliderInput(input_id, label = col,
                        min = min(vals, na.rm = TRUE),
                        max = max(vals, na.rm = TRUE),
                        value = range(vals, na.rm = TRUE)),
            checkboxInput(na_id, label = "Include NA", value = TRUE)
          )
        } else {
          # Initialize with "All"
          tagList(
            selectizeInput(input_id, label = col,
                           choices = c("All", "Has no data"),
                           selected = "All", multiple = TRUE),
            checkboxInput(na_id, label = "Include NA", value = TRUE)
          )
        }
      })
      do.call(tagList, ui_list)
    })
  })

  # Update choices for non-numeric filters
  observe({
    for(tbl in individual_tables) {
      cols <- dbListFields(con, tbl)
      for(col in cols) {
        if(col == "GeneID") next
        input_id <- paste(tbl, col, sep = "_")
        sample_val <- tryCatch({
          dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        }, error = function(e) NULL)
        if (!is.null(sample_val) && !is.numeric(sample_val)) {
          choices <- c("All", "Has no data", unique(dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]])[!is.na(unique(dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]]))])
          updateSelectizeInput(session, input_id, choices = choices, selected = "All", server = TRUE)
        }
      }
    }
  })

  filtered_data <- function(tbl) {
    reactive({
      cols <- dbListFields(con, tbl)
      conditions <- c()
      params <- list()
      for (col in cols) {
        if(col == "GeneID") next
        input_id <- paste(tbl, col, sep = "_")
        na_id <- paste(input_id, "na", sep = "_")
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        include_na <- isTRUE(input[[na_id]])
        if (is.numeric(sample_val)) {
          slider_val <- input[[input_id]]
          if (!is.null(slider_val)) {
            if (include_na) {
              conditions <- c(conditions, sprintf("(%s BETWEEN ? AND ? OR %s IS NULL)", col, col))
            } else {
              conditions <- c(conditions, sprintf("%s BETWEEN ? AND ?", col))
            }
            params <- c(params, slider_val[1], slider_val[2])
          }
        } else {
          input_val <- input[[input_id]]
          if (!is.null(input_val)) {
            if("Has no data" %in% input_val) {
              conditions <- c(conditions, sprintf("GeneID NOT IN (SELECT GeneID FROM %s WHERE %s IS NOT NULL)", tbl, col))
            } else if(length(input_val) > 0 && !("All" %in% input_val)) {
              placeholders <- paste(rep("?", length(input_val)), collapse = ", ")
              if(include_na) {
                conditions <- c(conditions, sprintf("(%s IN (%s) OR %s IS NULL)", col, placeholders, col))
              } else {
                conditions <- c(conditions, sprintf("%s IN (%s)", col, placeholders))
              }
              params <- c(params, input_val)
            } else {
              if (!include_na) {
                conditions <- c(conditions, sprintf("%s IS NOT NULL", col))
              }
            }
          }
        }
      }
      sql <- paste("SELECT * FROM", tbl)
      if (length(conditions) > 0) {
        sql <- paste(sql, "WHERE", paste(conditions, collapse = " AND "))
      }
      dbGetQuery(con, sql, params = params)
    }) |> debounce(500)
  }

  intersected_gene_ids <- reactive({
    filtered_ids <- lapply(individual_tables, function(tbl) {
      df <- filtered_data(tbl)()
      unique(df$GeneID)
    })
    if (length(filtered_ids) == 0) return(character(0))
    Reduce(intersect, filtered_ids)
  })

  output$aggregated_table <- DT::renderDataTable({
    req(input$agg_table)  # Ensure a table has been selected
    df <- dbReadTable(con, input$agg_table)

    DT::datatable(
      df,
      escape = FALSE,
      filter = "none",        # Remove per-column search bars
      rownames = FALSE,
      class = "display cell-border stripe",
      options = list(
        paging = TRUE,
        lengthChange = TRUE,  # Allow selection of number of rows
        searching = FALSE,    # Remove the global search bar
        autoWidth = TRUE,
        responsive = TRUE,
        scrollX = TRUE,
        dom = 't<"row"<"col-sm-4"l><"col-sm-4"i><"col-sm-4"p>>',  # Moves the length menu, info, and pagination to the bottom
        columnDefs = list(list(
          targets = "_all",
          render = JS("function(data, type, row, meta) {
                    if (type === 'display' && typeof data === 'string' && data.length > 10) {
                      return '<span title=\"' + data.replace(/\"/g, '&quot;') + '\">' + data.substr(0, 10) + '...</span>';
                    } else {
                      return data;
                    }
                  }")
        ))
      )
    )
  })

  observeEvent(input$bar_table, {
    cols <- dbListFields(con, input$bar_table)
    updateSelectInput(session, "bar_col", choices = c("", cols), selected = "")
  })

  valid_violin_tables <- reactive({
    valid <- sapply(individual_tables, function(tbl) {
      cols <- dbListFields(con, tbl)
      numeric_cols <- sapply(cols, function(col) {
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        is.numeric(sample_val)
      })
      any(numeric_cols)
    })
    individual_tables[valid]
  })

  output$violin_table_ui <- renderUI({
    valid_tables <- valid_violin_tables()
    if (length(valid_tables) == 0) {
      return(HTML("<em>No tables with valid numeric columns available for Violin/Box Plot</em>"))
    }
    selectInput("violin_table", "Select table for Violin/Box Plot:",
                choices = valid_tables,
                selected = valid_tables[1])
  })

  observeEvent(input$violin_table, {
    req(input$violin_table)
    tbl <- input$violin_table
    cols <- dbListFields(con, tbl)
    numeric_cols <- sapply(cols, function(col) {
      sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
      is.numeric(sample_val)
    })
    updateSelectInput(session, "violin_col", choices = c("", cols[numeric_cols]), selected = "")
  })

  observeEvent(input$scatter_table_x, {
    cols_x <- dbListFields(con, input$scatter_table_x)
    updateSelectInput(session, "scatter_x", choices = c("", cols_x), selected = "")
  })

  observeEvent(input$scatter_table_y, {
    cols_y <- dbListFields(con, input$scatter_table_y)
    updateSelectInput(session, "scatter_y", choices = c("", cols_y), selected = "")
  })

  observeEvent(input$stack_table_x, {
    cols <- dbListFields(con, input$stack_table_x)
    updateSelectInput(session, "stack_x", choices = c("", cols), selected = "")
  })

  valid_stack_y_tables <- reactive({
    valid <- sapply(individual_tables, function(tbl) {
      cols <- dbListFields(con, tbl)
      factor_cols <- sapply(cols, function(col) {
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        !is.numeric(sample_val)
      })
      any(factor_cols)
    })
    individual_tables[valid]
  })

  output$stack_table_y_ui <- renderUI({
    valid_tables <- valid_stack_y_tables()
    if (length(valid_tables) == 0) {
      return(HTML("<em>No tables with valid factor columns available for Stacked Bar Chart Y‑axis</em>"))
    }
    selectInput("stack_table_y", "Select table for Y:",
                choices = valid_tables,
                selected = valid_tables[1])
  })

  observeEvent(input$stack_table_y, {
    req(input$stack_table_y)
    tbl <- input$stack_table_y
    cols <- dbListFields(con, tbl)
    factor_cols <- sapply(cols, function(col) {
      sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
      !is.numeric(sample_val)
    })
    valid_factors <- cols[factor_cols]
    if(length(valid_factors) == 0) {
      updateSelectInput(session, "stack_y", choices = c(""))
    } else {
      updateSelectInput(session, "stack_y",
                        choices = c("", valid_factors),
                        selected = "")
    }
  })

  observe({
    choices <- c("Current List", names(saved_gene_lists$data))
    updateSelectizeInput(session, "bar_gene_lists", choices = choices, selected = choices, server = TRUE)
    updateSelectizeInput(session, "violin_gene_lists", choices = choices, selected = choices, server = TRUE)
    updateSelectizeInput(session, "scatter_gene_lists", choices = choices, selected = "Current List", server = TRUE)
    updateSelectizeInput(session, "stack_gene_list", choices = choices, selected = "Current List", server = TRUE)
    updateSelectizeInput(session, "upset_gene_lists", choices = choices, selected = choices, server = TRUE)
  })

  output$bar_bin_size_ui <- renderUI({
    req(input$bar_table, input$bar_col)
    if(input$bar_col == "") return(NULL)
    tbl <- input$bar_table
    col <- input$bar_col
    sample_val <- tryCatch({
      dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
    }, error = function(e) NULL)
    if(is.null(sample_val)) return(NULL)
    if(is.numeric(sample_val)) {
      vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
      min_val <- min(vals, na.rm = TRUE)
      max_val <- max(vals, na.rm = TRUE)
      default_bin_size <- (max_val - min_val) / 50
      numericInput("bar_bin_size", "Bin Size (for numeric X):",
                   value = default_bin_size,
                   min = default_bin_size / 10,
                   step = default_bin_size / 10)
    } else {
      NULL
    }
  })

  output$stack_bin_size_ui <- renderUI({
    req(input$stack_table_x, input$stack_x)
    if(input$stack_x == "") return(NULL)
    tbl <- input$stack_table_x
    col <- input$stack_x
    sample_val <- tryCatch({
      dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
    }, error = function(e) NULL)
    if(is.null(sample_val)) return(NULL)
    if(is.numeric(sample_val)) {
      vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
      min_val <- min(vals, na.rm = TRUE)
      max_val <- max(vals, na.rm = TRUE)
      default_bin_size <- (max_val - min_val) / 50
      numericInput("stack_bin_size", "Bin Size (for numeric X):",
                   value = default_bin_size,
                   min = default_bin_size / 10,
                   step = default_bin_size / 10)
    } else {
      NULL
    }
  })

  noDataPlot <- function() {
    plot_ly() %>%
      layout(title = "No data to plot",
             annotations = list(
               list(text = "No data to plot",
                    showarrow = FALSE,
                    x = 0.5, y = 0.5,
                    xref = "paper", yref = "paper")
             ))
  }

  # Reactive value to store the data frame used for plotting
  current_plot_df <- reactiveVal(NULL)

  plot_obj <- reactive({
    req(input$plot_type)
    if(input$plot_type == "Bar Chart/Histogram") {
      if (is.null(input$bar_table) || input$bar_table == "" ||
          is.null(input$bar_col) || input$bar_col == "" ||
          is.null(input$bar_gene_lists) || length(input$bar_gene_lists) == 0)
        return(noDataPlot())

      df <- filtered_data(input$bar_table)()
      if (nrow(df) == 0) return(noDataPlot())

      gene_ids <- intersected_gene_ids()
      if (length(gene_ids) == 0) return(noDataPlot())
      df <- df[df$GeneID %in% gene_ids, ]
      if(nrow(df) == 0) return(noDataPlot())

      combined <- do.call(rbind, lapply(input$bar_gene_lists, function(listName) {
        subset_genes <- if(listName == "Current List") intersected_gene_ids() else saved_gene_lists$data[[listName]]$genes
        df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
        if(nrow(df_subset)==0) return(NULL)
        df_subset$gene_list <- listName
        df_subset
      }))
      if(is.null(combined) || nrow(combined)==0) return(noDataPlot())

      if(!is.numeric(combined[[input$bar_col]])) {
        combined$value <- combined[[input$bar_col]]
        if(input$bar_show_na) {
          combined$value <- ifelse(is.na(combined$value), "Missing", combined$value)
        }
        if(input$bar_y_type=="percentage of genes") {
          counts <- combined %>%
            group_by(gene_list, value) %>%
            summarise(count = n_distinct(GeneID), .groups = "drop") %>%
            group_by(gene_list) %>%
            mutate(percentage = count/sum(count)*100) %>%
            ungroup()
          p <- plot_ly(data = counts, x = ~value, y = ~percentage, color = ~gene_list, type = "bar")
        } else {
          counts <- combined %>%
            group_by(gene_list, value) %>%
            summarise(count = n_distinct(GeneID), .groups = "drop")
          p <- plot_ly(data = counts, x = ~value, y = ~count, color = ~gene_list, type = "bar")
        }
      } else {
        non_missing <- combined[!is.na(combined[[input$bar_col]]), ]
        missing_count <- sum(is.na(combined[[input$bar_col]]))
        p <- plot_ly()
        if(nrow(non_missing) > 0) {
          bin_args <- if(!is.null(input$bar_bin_size)) list(size = input$bar_bin_size) else NULL
          p <- add_histogram(p, data = non_missing, x = ~get(input$bar_col),
                             color = ~gene_list,
                             histnorm = ifelse(input$bar_y_type=="percentage of genes", "percent", ""),
                             xbins = bin_args)
        }
        if(input$bar_show_na && missing_count > 0) {
          p <- add_trace(p, x = "Missing", y = missing_count, type = "bar", name = "Missing")
        }
      }

      p <- layout(p, barmode = "group",
                  title = paste("Bar Chart/Histogram of", input$bar_col),
                  xaxis = list(title = input$bar_col),
                  yaxis = list(title = ifelse(input$bar_y_type=="percentage of genes", "percentage of genes", "number of genes")))

      current_plot_df(combined)
      return(p)

    } else if(input$plot_type == "Violin/Box Plot") {
      if (is.null(input$violin_table) || input$violin_table == "" ||
          is.null(input$violin_col) || input$violin_col == "" ||
          is.null(input$violin_gene_lists) || length(input$violin_gene_lists) == 0)
        return(noDataPlot())

      df <- filtered_data(input$violin_table)()
      if (nrow(df) == 0) return(noDataPlot())

      gene_ids <- intersected_gene_ids()
      if (length(gene_ids) == 0) return(noDataPlot())
      df <- df[df$GeneID %in% gene_ids, ]
      if(nrow(df)==0) return(noDataPlot())

      combined <- do.call(rbind, lapply(input$violin_gene_lists, function(listName) {
        subset_genes <- if(listName=="Current List") intersected_gene_ids() else saved_gene_lists$data[[listName]]$genes
        df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
        if(nrow(df_subset)==0) return(NULL)
        df_subset$gene_list <- listName
        df_subset
      }))
      if(is.null(combined) || nrow(combined)==0) return(noDataPlot())

      df_non_missing <- combined[!is.na(combined[[input$violin_col]]), ]
      if(nrow(df_non_missing)==0) return(noDataPlot())

      gene_map <- gene_mapping()
      df_non_missing <- merge(df_non_missing, gene_map, by = "GeneID", all.x = TRUE)
      df_non_missing$hover_text <- paste("Gene:", df_non_missing$gene_symbol,
                                         "<br>", input$violin_col, ":", df_non_missing[[input$violin_col]])

      p <- plot_ly(data = df_non_missing, y = ~get(input$violin_col), color = ~gene_list, type = "violin",
                   box = list(visible = TRUE),
                   meanline = list(visible = TRUE),
                   points = ifelse(input$violin_show_points, "all", "outliers"),
                   text = ~hover_text,
                   hoverinfo = "text")
      p <- layout(p, title = paste("Violin/Box Plot of", input$violin_col),
                  yaxis = list(title = input$violin_col))

      current_plot_df(df_non_missing)
      return(p)

    } else if(input$plot_type == "Scatter Plot") {
      if (is.null(input$scatter_table_x) || input$scatter_table_x == "" ||
          is.null(input$scatter_table_y) || input$scatter_table_y == "" ||
          is.null(input$scatter_x) || input$scatter_x == "" ||
          is.null(input$scatter_y) || input$scatter_y == "" ||
          is.null(input$scatter_gene_lists) || length(input$scatter_gene_lists)==0)
        return(noDataPlot())

      gene_ids <- intersected_gene_ids()
      if(length(gene_ids)==0) return(noDataPlot())

      if (input$scatter_table_x == input$scatter_table_y) {
        df_joint <- filtered_data(input$scatter_table_x)()
        df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
        if(nrow(df_joint)==0) return(noDataPlot())
        if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
          return(noDataPlot())
      } else {
        df_x <- filtered_data(input$scatter_table_x)()
        df_y <- filtered_data(input$scatter_table_y)()
        df_x <- df_x[df_x$GeneID %in% gene_ids, ]
        df_y <- df_y[df_y$GeneID %in% gene_ids, ]
        df_joint <- inner_join(df_x, df_y, by = "GeneID")
        if(nrow(df_joint)==0) return(noDataPlot())
        if(!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
          return(noDataPlot())
      }

      df_joint <- df_joint[!is.na(df_joint[[input$scatter_x]]) & !is.na(df_joint[[input$scatter_y]]), ]

      combined <- do.call(rbind, lapply(input$scatter_gene_lists, function(listName) {
        subset_genes <- if(listName == "Current List") intersected_gene_ids() else saved_gene_lists$data[[listName]]$genes
        df_subset <- df_joint[df_joint$GeneID %in% subset_genes, , drop = FALSE]
        if(nrow(df_subset)==0) return(NULL)
        df_subset$gene_list <- listName
        df_subset
      }))
      if(is.null(combined) || nrow(combined)==0) return(noDataPlot())

      gene_map <- gene_mapping()
      combined <- merge(combined, gene_map, by = "GeneID", all.x = TRUE)
      combined$hover_text <- paste("Gene:", combined$gene_symbol)

      p <- plot_ly(data = combined,
                   x = ~get(input$scatter_x),
                   y = ~get(input$scatter_y),
                   type = "scatter",
                   mode = "markers",
                   color = ~gene_list,
                   text = ~hover_text,
                   hoverinfo = "text+x+y") %>%
        layout(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y),
               xaxis = list(title = input$scatter_x),
               yaxis = list(title = input$scatter_y))

      current_plot_df(combined)
      return(p)

    } else if(input$plot_type == "Stacked Bar Chart") {
      if (is.null(input$stack_table_x) || input$stack_table_x == "" ||
          is.null(input$stack_x) || input$stack_x == "" ||
          is.null(input$stack_table_y) || input$stack_table_y == "" ||
          is.null(input$stack_y) || input$stack_y == "" ||
          is.null(input$stack_gene_list) || input$stack_gene_list == "")
        return(noDataPlot())

      if(input$stack_table_x == input$stack_table_y) {
        df_joint <- filtered_data(input$stack_table_x)()
      } else {
        df_x <- filtered_data(input$stack_table_x)()
        df_y <- filtered_data(input$stack_table_y)()
        df_joint <- inner_join(df_x, df_y, by = "GeneID")
      }
      if(nrow(df_joint)==0) return(noDataPlot())

      gene_ids <- intersected_gene_ids()
      if(length(gene_ids)==0) return(noDataPlot())
      df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
      if(nrow(df_joint)==0) return(noDataPlot())

      if(input$stack_gene_list != "Current List") {
        subset_genes <- saved_gene_lists$data[[input$stack_gene_list]]$genes
        df_joint <- df_joint[df_joint$GeneID %in% subset_genes, ]
        if(nrow(df_joint)==0) return(noDataPlot())
      }

      xcol <- df_joint[[input$stack_x]]
      ycol <- df_joint[[input$stack_y]]

      if (is.numeric(xcol)) {
        bin_size <- if (!is.null(input$stack_bin_size)) input$stack_bin_size else ((max(xcol, na.rm = TRUE) - min(xcol, na.rm = TRUE)) / 50)
        bins <- seq(min(xcol, na.rm = TRUE), max(xcol, na.rm = TRUE) + bin_size, by = bin_size)
        df_joint$bin <- cut(xcol, breaks = bins, include.lowest = TRUE, right = FALSE)
        if(input$stack_show_na_x) {
          df_joint$bin <- as.character(df_joint$bin)
          df_joint$bin[is.na(xcol)] <- "Missing"
        }
      } else {
        df_joint$bin <- as.factor(xcol)
        if(input$stack_show_na_x) {
          df_joint$bin <- as.character(df_joint$bin)
          df_joint$bin[is.na(xcol)] <- "Missing"
        }
      }

      df_joint$group <- as.factor(ycol)
      if(input$stack_show_na_y) {
        df_joint$group <- as.character(df_joint$group)
        df_joint$group[is.na(ycol)] <- "Missing"
      }

      summary_df <- df_joint %>%
        group_by(bin, group) %>%
        summarise(count = n(), .groups = "drop") %>%
        ungroup()
      if(input$stack_y_type == "percentage of genes") {
        summary_df <- summary_df %>%
          group_by(bin) %>%
          mutate(percentage = count / sum(count) * 100) %>%
          ungroup()
        y_val <- summary_df$percentage
        y_title <- "percentage of genes"
      } else {
        y_val <- summary_df$count
        y_title <- "number of genes"
      }

      p <- plot_ly(data = summary_df, x = ~bin, y = ~y_val, color = ~group, type = "bar") %>%
        layout(title = paste("Stacked Bar Chart: % of", input$stack_y, "by", input$stack_x),
               xaxis = list(title = input$stack_x),
               yaxis = list(title = y_title),
               barmode = "stack")

      current_plot_df(summary_df)
      return(p)
    }
  })

  output$plot_ui <- renderUI({
    if (input$plot_type == "UpSet Plot") {
      withSpinner(plotOutput("upset_plot"))
    } else {
      withSpinner(card(plotlyOutput("plot_output"), full_screen = TRUE))
    }
  })

  output$plot_output <- renderPlotly({
    plot_obj()
  })

  output$plot_data_table <- DT::renderDataTable({
    if (input$plot_type == "UpSet Plot") {
      DT::datatable(data.frame(Message = "Data table not available for UpSet Plot"), options = list(dom = 't'))
    } else {
      df <- current_plot_df()
      if (is.null(df) || nrow(df) == 0) {
        DT::datatable(data.frame(Message = "No data available for table"), options = list(dom = 't'))
      } else {
        DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
      }
    }
  })

  output$download_plot_df <- downloadHandler(
    filename = function() {
      paste0("plot_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- current_plot_df()
      if(!is.null(df) && nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message="No plot data available"), file, row.names = FALSE)
      }
    }
  )

  gene_list_sets <- reactive({
    available <- c("Current List", names(saved_gene_lists$data))
    sel <- input$upset_gene_lists
    if(is.null(sel) || length(sel)==0) sel <- available
    sets <- list()
    if("Current List" %in% sel)
      sets[["Current List"]] <- intersected_gene_ids()
    for(n in names(saved_gene_lists$data)) {
      if(n %in% sel)
        sets[[n]] <- saved_gene_lists$data[[n]]$genes
    }
    sets
  })

  output$upset_plot <- renderPlot({
    req(input$plot_type == "UpSet Plot")
    sets <- gene_list_sets()
    if(length(sets) < 2 || all(sapply(sets, length) < 2)) {
      plot.new()
      text(0.5, 0.5, "Select two lists to show UpSet Plot.")
      return()
    }
    m <- fromList(sets)
    upset(m, order.by = "freq")
  })

  output$download_gene_lists_ui <- renderUI({
    if(length(saved_gene_lists$data) == 0){
      HTML("<em>Save gene lists to be able to download them</em>")
    } else {
      tagList(
        selectInput("gene_list_file_type", "Select file type for saved gene lists:",
                    choices = c("CSV", "TSV"), selected = "CSV"),
        downloadButton("download_gene_lists", "Download Saved Gene Lists")
      )
    }
  })

  # Use the plus button in Saved Gene Lists accordion to trigger the save modal.
  observeEvent(input$add_gene_list, {
    showModal(modalDialog(
      title = "Save Gene List",
      textInput("gene_list_name", "Enter a name for this gene list:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Save")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_save, {
    req(input$gene_list_name)
    current_genes <- intersected_gene_ids()
    current_filters <- reactiveValuesToList(input)
    filter_keys <- names(current_filters)[sapply(names(current_filters), function(x) {
      any(sapply(individual_tables, function(tbl) {
        startsWith(x, paste0(tbl, "_"))
      }))
    })]
    saved_filters <- current_filters[filter_keys]
    saved_gene_lists$data[[input$gene_list_name]] <- list(
      genes = current_genes,
      filters = saved_filters
    )
    removeModal()
  })

  observe({
    choices <- c("Current List", names(saved_gene_lists$data))
    updateSelectizeInput(session, "scatter_gene_lists", choices = choices, selected = "Current List", server = TRUE)
    updateSelectizeInput(session, "stack_gene_list", choices = choices, selected = "Current List", server = TRUE)
  })

  observeEvent(input$show_filters, {
    showModal(modalDialog(
      title = "Current Filters",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      uiOutput("current_filters")
    ))
  })

  observeEvent(input$clear_filters, {
    for(tbl in individual_tables) {
      cols <- dbListFields(con, tbl)
      for(col in cols) {
        if(col == "GeneID") next
        input_id <- paste(tbl, col, sep = "_")
        na_id <- paste(input_id, "na", sep = "_")
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        if(is.numeric(sample_val)) {
          vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
          default_val <- range(vals, na.rm = TRUE)
          updateSliderInput(session, input_id, value = default_val)
        } else {
          updateSelectizeInput(session, input_id, selected = "All")
        }
        updateCheckboxInput(session, na_id, value = TRUE)
      }
    }
  })

  ## --- LIST INPUT MODAL LOGIC (for custom filter lists) ---
  observeEvent(input$list_input, {
    showModal(modalDialog(
      title = "Filter custom list (genes, phenotypes etc)",
      fluidPage(
        fluidRow(h5('Select Table and Column to filter')),
        fluidRow(
          column(6,
                 selectInput("list_input_table", "Table:",
                             choices = individual_tables, selected = individual_tables[1])
          ),
          column(6,
                 uiOutput("list_input_column_ui")
          )
        ),
        hr(),
        fluidRow(h5('Input custom list')),
        fluidRow(
          column(12,
                 textInput("list_input_text", NULL, value = "")
          )
        ),
        hr(),
        fluidRow(h5('Select list delimiter')),
        fluidRow(
          column(12,
                 selectInput("list_input_separator", NULL,
                             choices = c("Semicolon (;)" = ";",
                                         "Comma (,)" = ",",
                                         "Whitespace" = " ",
                                         "Pipe (|)" = "|"),
                             selected = ";")
          )
        ),
        # fluidRow(
        #   column(12,
        #          actionButton("apply_list_input", "Apply", class = "btn-primary")
        #   )
        # ),
        fluidRow(
          column(12,
                 htmlOutput("list_input_message")
          )
        )
      ),
      easyClose = TRUE,
      # footer =
       footer = tagList(
         actionButton("apply_list_input", "Apply", class = "btn-primary"),
          modalButton("Close")
        )
    ))
  })

  output$list_input_column_ui <- renderUI({
    req(input$list_input_table)
    tbl <- input$list_input_table
    cols <- setdiff(dbListFields(con, tbl), "GeneID")
    selectInput("list_input_column", "Column:", choices = cols, selected = cols[1])
  })

  observeEvent(input$apply_list_input, {
    req(input$list_input_table, input$list_input_column, input$list_input_text, input$list_input_separator)
    tbl <- input$list_input_table
    col <- input$list_input_column
    filter_input_id <- paste(tbl, col, sep = "_")

    # Split text input by chosen separator
    entries <- unlist(strsplit(input$list_input_text, split = input$list_input_separator, fixed = TRUE))
    entries <- trimws(entries)
    entries <- entries[entries != ""]

    # Get available values for the column from database
    available_values <- as.character(dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]])
    available_values <- available_values[!is.na(available_values)]

    # For each entry, perform case-insensitive matching and return the matched value (preserving original case)
    valid_entries <- sapply(entries, function(x) {
      matches <- available_values[tolower(available_values) == tolower(x)]
      if(length(matches) > 0) matches[1] else NA
    })
    valid_entries <- valid_entries[!is.na(valid_entries)]
    invalid_entries <- entries[!(tolower(entries) %in% tolower(available_values))]

    updateSelectizeInput(session, filter_input_id,
                         choices = c("All", "Has no data", available_values),
                         selected = if(length(valid_entries)==0) "All" else valid_entries)

    updateTextInput(session, "list_input_text", value = "")

    message_parts <- c()
    if (length(valid_entries) > 0) {
      message_parts <- c(message_parts, paste("Matched terms:", paste(valid_entries, collapse = ", ")))
    }
    if (length(invalid_entries) > 0) {
      message_parts <- c(message_parts, paste("The following entries did not match any available terms:",
                                              paste(invalid_entries, collapse = ", ")))
    }

    output$list_input_message <- renderUI({
      HTML(paste(message_parts, collapse = "<br>"))
    })
  })

  output$download_agg_table <- downloadHandler(
    filename = function() {
      paste0("aggregated_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      gene_ids <- intersected_gene_ids()
      if(length(gene_ids) == 0) {
        write.csv(data.frame(Message = "No matching gene IDs"), file, row.names = FALSE)
      } else {
        cols_to_show <- if (!is.null(input$agg_columns) && length(input$agg_columns) > 0) {
          paste(input$agg_columns, collapse = ", ")
        } else {
          "*"
        }
        placeholders <- paste(rep("?", length(gene_ids)), collapse = ", ")
        sql <- paste("SELECT", cols_to_show, "FROM aggregated WHERE GeneID IN (", placeholders, ")", sep = " ")
        df <- dbGetQuery(con, sql, params = gene_ids)
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
