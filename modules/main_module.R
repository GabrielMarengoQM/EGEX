# main_module.R

# Module UI function
mainModuleUI <- function(id, all_tables, individual_tables) {
  ns <- NS(id)
  tagList(
    div(
      class = "custom-sidebar",
      page_sidebar(
        #____________________________________________________________________#
        ##### +++++++++++++++++++++++++ SideBar +++++++++++++++++++++++++ ####
        #--------------------------------------------------------------------#
        sidebar = sidebar(
          width = "500px",
          open = c("open"),
          div(class = "overflow-auto", style = "max-height: 85vh;",
              # SAVED GENE LISTS UI ----
              accordion(
                accordion_panel(tagList(icon("list"), "Saved Gene Lists"), uiOutput(ns("saved_gene_lists_ui")),
                                actionButton(ns("add_gene_list"), "+ Save current gene list", class = "btn btn-success"), value = "saved"),
                ##### ========================= Table & plot options ========================= #####
                # TABLE/PLOT OPTIONS ----
                accordion_panel(
                  tagList(icon("columns"), "Gene Table/Plot Options"),
                  tagList(
                    ##### ========================= Table options ========================= #####
                    accordion(
                      # TABLE OPTIONS ----
                      accordion_panel(
                        tagList(icon("table"), "Gene Table Options"),
                        tagList(
                          selectInput(
                            ns("agg_table"),
                            "Table to display:",
                            choices = all_tables,
                            selected = "aggregated"
                          )
                        ),
                        value = "table_options"
                      ),
                      ##### ========================= Plot options ========================= #####
                      # PLOT OPTIONS ----
                      accordion_panel(
                        tagList(icon("sliders"), "Plot Options"),
                        tagList(
                          selectInput(
                            ns("plot_type"),
                            "Select Plot Type:",
                            choices = c(
                              "Bar Chart/Histogram",
                              "Violin/Box Plot",
                              "Scatter Plot",
                              "Stacked Bar Chart",
                              "UpSet Plot"
                            )
                          ),
                          ##### ========================= Bar/Histogram ========================= #####
                          conditionalPanel(
                            # Bar/histogram options ui ----
                            condition = sprintf("input['%s'] == 'Bar Chart/Histogram'", ns("plot_type")),
                            hr(),
                            fluidRow(
                              column(6,
                                     selectInput(
                                       ns("bar_table"),
                                       "Select Table:",
                                       choices = individual_tables,
                                       selected = individual_tables[1]
                                     )
                              ),
                              column(6,
                                     selectInput(ns("bar_col"), "Select Column:", choices = NULL)
                              )
                            ),
                            hr(),
                            selectizeInput(ns("bar_gene_lists"), "Select Gene List(s) to plot:", choices = NULL, multiple = TRUE),
                            hr(),
                            uiOutput(ns("bar_bin_size_ui")),
                            checkboxInput(ns("bar_show_na"), "Plot missing values", value = FALSE),
                            hr(),
                            radioButtons(
                              ns("bar_y_type"),
                              "Y-axis type:",
                              choices = c("number of genes", "% of genes"),
                              selected = "number of genes",
                              inline = TRUE
                            ),
                            numericInput(ns("bar_y_threshold1"), "Y-axis Threshold 1 (optional):", value = NA, step = 1),
                            numericInput(ns("bar_y_threshold2"), "Y-axis Threshold 2 (optional):", value = NA, step = 1)
                          ),
                          ##### ========================= Box/Violin ========================= #####
                          conditionalPanel(
                            # Violin options ui ----
                            condition = sprintf("input['%s'] == 'Violin/Box Plot'", ns("plot_type")),
                            hr(),
                            fluidRow(
                              column(6,
                                     uiOutput(ns("violin_table_ui"))
                              ),
                              column(6,
                                     # selectizeInput(ns("violin_col"), "Select numeric column:", choices = NULL, multiple = TRUE)
                                     shinyWidgets::pickerInput(
                                       inputId = ns("violin_col"),
                                       label = "Select numeric column:",
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = list(
                                         `actions-box` = TRUE,              # ✅ enables "Select All"
                                         `live-search` = TRUE,              # 🔍 search box inside dropdown
                                         `selected-text-format` = "count > 3"
                                       )
                                     )
                              )
                            ),
                            hr(),
                            selectizeInput(ns("violin_gene_lists"), "Select Gene List(s) to plot:", choices = NULL, multiple = TRUE),
                            hr(),
                            checkboxInput(ns("violin_show_points"), "Show all points", value = FALSE),
                            hr(),
                            # Two Y thresholds for Violin/Box Plot:
                            numericInput(ns("violin_y_threshold1"), "Y-axis Threshold 1 (optional):", value = NA, step = 1),
                            numericInput(ns("violin_y_threshold2"), "Y-axis Threshold 2 (optional):", value = NA, step = 1),
                            hr(),
                            radioButtons(ns("violin_plot_style"), "Plot Style:",
                                         choices = c("Violin Plot" = "violin",
                                                     "Box Plot" = "box",
                                                     "Violin with Box Plot" = "violin_box"),
                                         selected = "violin_box",
                                         inline = TRUE)
                          ),
                          ##### ========================= Scatter ========================= #####
                          conditionalPanel(
                            # Scatter options ui ----
                            condition = sprintf("input['%s'] == 'Scatter Plot'", ns("plot_type")),
                            hr(),
                            fluidRow(
                              column(6,
                                     selectInput(
                                       ns("scatter_table_x"),
                                       "Select table for X:",
                                       choices = individual_tables,
                                       selected = individual_tables[1]
                                     )
                              ),
                              column(6,
                                     selectInput(
                                       ns("scatter_table_y"),
                                       "Select table for Y:",
                                       choices = individual_tables,
                                       selected = individual_tables[1]
                                     )
                              )
                            ),
                            fluidRow(
                              column(6,
                                     selectInput(ns("scatter_x"), "Select X column:", choices = NULL)
                              ),
                              column(6,
                                     selectInput(ns("scatter_y"), "Select Y column:", choices = NULL)
                              )
                            ),
                            hr(),
                            selectizeInput(ns("scatter_gene_lists"), "Select Gene List(s) to plot:", choices = NULL, selected = "Current List", multiple = TRUE),
                            hr(),
                            numericInput(ns("scatter_x_threshold1"), "X-axis Threshold 1 (optional):", value = NA, step = 1),
                            numericInput(ns("scatter_x_threshold2"), "X-axis Threshold 2 (optional):", value = NA, step = 1),
                            numericInput(ns("scatter_y_threshold1"), "Y-axis Threshold 1 (optional):", value = NA, step = 1),
                            numericInput(ns("scatter_y_threshold2"), "Y-axis Threshold 2 (optional):", value = NA, step = 1)
                          ),
                          conditionalPanel(
                            ##### ========================= Stacked bar ========================= #####
                            # Stacked bar options ui ----
                            condition = sprintf("input['%s'] == 'Stacked Bar Chart'", ns("plot_type")),
                            hr(),
                            fluidRow(
                              column(6,
                                     selectInput(
                                       ns("stack_table_x"),
                                       "Select table for X:",
                                       choices = individual_tables,
                                       selected = individual_tables[1]
                                     )
                              ),
                              column(6,
                                     uiOutput(ns("stack_table_y_ui"))
                              )
                            ),
                            fluidRow(
                              column(6,
                                     selectInput(ns("stack_x"), "Select X column:", choices = NULL)
                              ),
                              column(6,
                                     selectInput(ns("stack_y"), "Select Y column:", choices = NULL)
                              )
                            ),
                            fluidRow(
                              column(6,uiOutput(ns("stack_bin_size_ui")))
                            ),
                            hr(),
                            selectizeInput(ns("stack_gene_list"), "Select Gene List(s) to plot:", choices = NULL, selected = "Current List"),
                            hr(),
                            radioButtons(
                              ns("stack_y_type"),
                              "Y-axis display:",
                              choices = c("number of genes", "% of genes"),
                              selected = "% of genes",
                              inline = TRUE
                            ),
                            hr(),
                            fluidRow(
                              column(6,
                                     checkboxInput(ns("stack_show_na_x"), "Show missing X", value = FALSE)
                              ),
                              column(6,
                                     checkboxInput(ns("stack_show_na_y"), "Show missing Y", value = FALSE)
                              )
                            ),
                            hr(),
                            # Remove any X threshold here; add two Y thresholds only:
                            numericInput(ns("stack_y_threshold1"), "Y-axis Threshold 1 (optional):", value = NA, step = 1),
                            numericInput(ns("stack_y_threshold2"), "Y-axis Threshold 2 (optional):", value = NA, step = 1)
                          ),
                          hr(),
                          selectInput(ns("color_palette"), "Select Color Palette:",
                                      choices = c("NPG", "AAAS", "NEJM", "Lancet", "JAMA", "BMJ", "JCO",
                                                  "UCSCGB", "D3", "Observable", "LocusZoom", "IGV", "COSMIC",
                                                  "UChicago", "Star Trek", "Tron Legacy", "Futurama",
                                                  "Rick and Morty", "The Simpsons", "Flat UI", "Frontiers",
                                                  "GSEA", "Bootstrap 5", "Material Design", "Tailwind CSS"),
                                      selected = "NPG"),
                          ##### ========================= Upset ========================= #####
                          conditionalPanel(
                            # Upset options ui ----
                            condition = sprintf("input['%s'] == 'UpSet Plot'", ns("plot_type")),
                            selectizeInput(ns("upset_gene_lists"), "Select Gene Lists to plot:", choices = NULL, multiple = TRUE)
                          )
                        ),
                        value = "plot_options"
                      ),
                      open = "plot_options"
                    )
                  ),
                  value = "options"
                ),
                ##### ========================= Filters ========================= #####
                # FILTERS UI ----
                accordion_panel(
                  tagList(icon("filter"), "Filter Controls"),
                  tagList(
                    fluidRow(
                      column(6, actionButton(ns("clear_filters"), "Clear Filters", width = "100%")),
                      column(6, actionButton(ns("show_filters"), "View Filters", width = "100%"))
                    ),
                    hr(),
                    fluidRow(
                      column(12, actionButton(ns("list_input"), "Input custom list", width = "100%", class = "btn-primary"))
                    ),
                    hr(),
                    accordion(
                      lapply(individual_tables, function(tbl) {
                        accordion_panel(
                          title = tbl,
                          value = tbl,
                          uiOutput(ns(paste0("filters_", tbl)))
                        )
                      }),
                      open = FALSE
                    ),
                  ),
                  value = "controls"
                ),
                open = "controls"
              )
          )
        ),
        #____________________________________________________________________#
        ##### +++++++++++++++++++++++ Main Panel ++++++++++++++++++++++++ ####
        #--------------------------------------------------------------------#
        # MAIN PANEL ----
        tabsetPanel(
          tabPanel("Gene Table",
                   ##### ========================= Gene Table ========================= #####
                   # TABLE TAB ----
                   withSpinner(card(DT::dataTableOutput(ns("aggregated_table")), full_screen = TRUE, height = 625)),
                   fluidRow(
                     column(3, downloadButton(ns("download_agg_table"), "Download Table"))
                   )
          ),
          ##### ========================= Plot ========================= #####
          # PLOT TAB ---
          tabPanel("Plot",
                   # Updated card code for plot output; wrapped with spinner
                   withSpinner(uiOutput(ns("plot_ui"))),
                   # DT table and download button for plot data, wrapped with spinner
                   withSpinner(DT::dataTableOutput(ns("plot_data_table"))),
                   fluidRow(
                     column(3, downloadButton(ns("download_plot_df"), "Download Plot Data"))
                   )
          )
        )
        
      )
    )
  )
}

# Module Server function
mainModuleServer <- function(id, con, individual_tables, saved_gene_lists) {
  moduleServer(id, function(input, output, session) {
    #____________________________________________________________________#
    ##### +++++++++++++++++++++++++ SideBar +++++++++++++++++++++++++ ####
    #--------------------------------------------------------------------#
    
    ##### ========================= Reactive value for GeneID & Symbol mappings ========================= #####
    ### Reactive gene mapping from the "genes" table (assumes columns 'GeneID' and 'symbol') (for showing symbols in Plotly Traces)
    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })
    
    ######################################################################
    ##### ==================== Saved Gene Lists ===================== ####
    ######################################################################
    
    ##### ========================= Save gene lists (UI) ========================= #####
    output$saved_gene_lists_ui <- renderUI({
      if(length(saved_gene_lists$data) == 0) {
        HTML("<em>No saved gene lists.</em>")
      } else {
        tagList(
          lapply(names(saved_gene_lists$data), function(name) {
            count <- length(saved_gene_lists$data[[name]]$genes)
            applyId <- paste0("apply_", gsub(" ", "_", name))
            removeId <- paste0("remove_", gsub(" ", "_", name))
            fluidRow(
              column(3, strong(name)),
              column(3, paste("Genes:", count)),
              column(3, actionButton(session$ns(applyId), "Apply Filters", class = "btn-sm")),
              column(3, actionButton(session$ns(removeId), "Remove", class = "btn-danger btn-sm"))
            )
          })
        )
      }
    })
    
    ##### ========================= Save gene list Modal (UI) ========================= #####
    observeEvent(input$add_gene_list, {
      showModal(modalDialog(
        title = "Save Gene List",
        textInput(session$ns("gene_list_name"), "Enter a name for this gene list:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_save"), "Save")
        ),
        easyClose = TRUE
      ))
    })
    
    ##### ========================= Save Gene List (Server) ========================= #####
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
    
    ##### ========================= "Apply filters" Button (Server) ========================= #####
    observe({
      req(saved_gene_lists$data)
      for(name in names(saved_gene_lists$data)) {
        local({
          listName <- name
          # Dynamically namespace the button ID
          applyId <- paste0("apply_", gsub(" ", "_", name))
          observeEvent(input[[applyId]], {
            saved_filters <- saved_gene_lists$data[[listName]]$filters
            for(key in names(saved_filters)) {
              val <- saved_filters[[key]]
              if (is.logical(val)) {
                updateSwitchInput(session, key, value = val, onLabel = "include", offLabel = "exclude")
              } else if (is.numeric(val)) {
                updateSliderInput(session, key, value = val)
              } else {
                updateSelectInput(session, key, selected = val)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    ##### ========================= Remove saved gene list (Server) ========================= #####
    observe({
      req(saved_gene_lists$data)
      for(name in names(saved_gene_lists$data)) {
        local({
          listName <- name
          removeId <- paste0("remove_", gsub(" ", "_", name))
          observeEvent(input[[removeId]], {
            saved_gene_lists$data[[listName]] <- NULL
          }, ignoreInit = TRUE)
        })
      }
    })
    
    ########################################################################
    ##### ================= Gene Table/Plot Options ================= ####
    ########################################################################
    
    ##### ========================= Observe Bar/Histogram Table choice in Plot Options ========================= #####
    observeEvent(input$bar_table, {
      cols <- dbListFields(con, input$bar_table)
      updateSelectInput(session, "bar_col", choices = c("", cols), selected = "")
    })
    
    ##### ========================= Update Bar Chart Bin Size based on input ========================= #####
    output$bar_bin_size_ui <- renderUI({
      req(input$bar_table, input$bar_col)
      if (input$bar_col == "") return(NULL)
      tbl <- input$bar_table
      col <- input$bar_col
      sample_val <- tryCatch({
        dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
      }, error = function(e) NULL)
      if (is.null(sample_val)) return(NULL)
      if (is.numeric(sample_val)) {
        vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
        min_val <- min(vals, na.rm = TRUE)
        max_val <- max(vals, na.rm = TRUE)
        default_bin_size <- (max_val - min_val) / 50
        numericInput(session$ns("bar_bin_size"), "Bin Size (for numeric X):",
                     value = default_bin_size,
                     min = default_bin_size / 10,
                     step = default_bin_size / 10)
      } else {
        NULL
      }
    })
    
    ##### ========================= Only Display Numeric choices for Violin/Box plots (Server)  ========================= #####
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
    
    ##### ========================= Render Table options for Violin/Box plots (UI) ========================= #####
    output$violin_table_ui <- renderUI({
      valid_tables <- valid_violin_tables()
      if (length(valid_tables) == 0) {
        return(HTML("<em>No tables with valid numeric columns available for Violin/Box Plot</em>"))
      }
      selectInput(session$ns("violin_table"), "Select table for Plot:",
                  choices = valid_tables,
                  selected = valid_tables[1])
    })
    
    ##### ========================= Violin/Box Table choice: Only display Numeric Column choices ========================= #####
    observeEvent(input$violin_table, {
      req(input$violin_table)
      tbl <- input$violin_table
      cols <- dbListFields(con, tbl)
      numeric_cols <- sapply(cols, function(col) {
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        is.numeric(sample_val)
      })
      # updateSelectInput(session, "violin_col", choices = c("", cols[numeric_cols]), selected = "")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "violin_col",
        choices = c("", cols[numeric_cols]),
        selected = ""
      )
    })
    
    ##### ========================= Scatter plot X axis: Change Column choices based on Table choice ========================= #####
    observeEvent(input$scatter_table_x, {
      cols_x <- dbListFields(con, input$scatter_table_x)
      updateSelectInput(session, "scatter_x", choices = c("", cols_x), selected = "")
    })
    
    ##### ========================= Scatter plot Y axis: Change Column choices based on Table choice ========================= #####
    observeEvent(input$scatter_table_y, {
      cols_y <- dbListFields(con, input$scatter_table_y)
      updateSelectInput(session, "scatter_y", choices = c("", cols_y), selected = "")
    })
    
    ##### ========================= Stacked Bar plot X axis: Change Column choices based on Table choice ========================= #####
    observeEvent(input$stack_table_x, {
      cols <- dbListFields(con, input$stack_table_x)
      updateSelectInput(session, "stack_x", choices = c("", cols), selected = "")
    })
    
    ##### ========================= Stacked Bar Table choice Y axis: Only display Numeric Column choices 1 (Function) ========================= #####
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
    
    ##### ========================= Stacked Bar Table choice Y axis: Only display Numeric Column choices 2 (Apply Func) ========================= #####
    output$stack_table_y_ui <- renderUI({
      valid_tables <- valid_stack_y_tables()
      if (length(valid_tables) == 0) {
        return(HTML("<em>No tables with valid factor columns available for Stacked Bar Chart Y‑axis</em>"))
      }
      selectInput(session$ns("stack_table_y"), "Select table for Y:",
                  choices = valid_tables,
                  selected = valid_tables[1])
    })
    
    ##### ========================= Stacked Bar Column choice Y axis: Only display Numeric Column choices  ========================= #####
    observeEvent(input$stack_table_y, {
      req(input$stack_table_y)
      tbl <- input$stack_table_y
      cols <- dbListFields(con, tbl)
      factor_cols <- sapply(cols, function(col) {
        sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
        !is.numeric(sample_val)
      })
      valid_factors <- cols[factor_cols]
      if (length(valid_factors) == 0) {
        updateSelectInput(session, "stack_y", choices = c(""))
      } else {
        updateSelectInput(session, "stack_y",
                          choices = c("", valid_factors),
                          selected = "")
      }
    })
    
    ##### ========================= Update Stacked Bar Chart Bin Size based on input ========================= #####
    output$stack_bin_size_ui <- renderUI({
      req(input$stack_table_x, input$stack_x)
      if (input$stack_x == "") return(NULL)
      tbl <- input$stack_table_x
      col <- input$stack_x
      sample_val <- tryCatch({
        dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
      }, error = function(e) NULL)
      if (is.null(sample_val)) return(NULL)
      if (is.numeric(sample_val)) {
        vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
        min_val <- min(vals, na.rm = TRUE)
        max_val <- max(vals, na.rm = TRUE)
        default_bin_size <- (max_val - min_val) / 50
        numericInput(session$ns("stack_bin_size"), "Bin Size (for numeric X):",
                     value = default_bin_size,
                     min = default_bin_size / 10,
                     step = default_bin_size / 10)
      } else {
        NULL
      }
    })
    
    ##### ========================= Update Gene Lists in Selection in Plot Options  ========================= #####
    observe({
      choices <- c("Current List", names(saved_gene_lists$data))
      updateSelectizeInput(session, "bar_gene_lists", choices = choices, selected = choices, server = TRUE)
      updateSelectizeInput(session, "violin_gene_lists", choices = choices, selected = choices, server = TRUE)
      updateSelectizeInput(session, "scatter_gene_lists", choices = choices, selected = "Current List", server = TRUE)
      updateSelectizeInput(session, "stack_gene_list", choices = choices, selected = "Current List", server = TRUE)
      updateSelectizeInput(session, "upset_gene_lists", choices = choices, selected = choices, server = TRUE)
    })
    
    ##### ========================= Update Gene List selection for Scatter and Stack in Plot Options ========================= #####
    observe({
      choices <- c("Current List", names(saved_gene_lists$data))
      updateSelectizeInput(session, "scatter_gene_lists", choices = choices, selected = "Current List", server = TRUE)
      updateSelectizeInput(session, "stack_gene_list", choices = choices, selected = "Current List", server = TRUE)
    })
    
    ##### ========================= GGSCI Plot Palette Options ========================= #####
    get_palette <- function(palette_name, n) {
      if (palette_name == "NPG") {
        return(ggsci::pal_npg()(n))
      } else if (palette_name == "AAAS") {
        return(ggsci::pal_aaas()(n))
      } else if (palette_name == "NEJM") {
        return(ggsci::pal_nejm()(n))
      } else if (palette_name == "Lancet") {
        return(ggsci::pal_lancet()(n))
      } else if (palette_name == "JAMA") {
        return(ggsci::pal_jama()(n))
      } else if (palette_name == "BMJ") {
        return(ggsci::pal_bmj()(n))
      } else if (palette_name == "JCO") {
        return(ggsci::pal_jco()(n))
      } else if (palette_name == "UCSCGB") {
        return(ggsci::pal_ucscgb()(n))
      } else if (palette_name == "D3") {
        return(ggsci::pal_d3()(n))
      } else if (palette_name == "Observable") {
        return(ggsci::pal_observable()(n))
      } else if (palette_name == "LocusZoom") {
        return(ggsci::pal_locuszoom()(n))
      } else if (palette_name == "IGV") {
        return(ggsci::pal_igv()(n))
      } else if (palette_name == "COSMIC") {
        return(ggsci::pal_cosmic()(n))
      } else if (palette_name == "UChicago") {
        return(ggsci::pal_uchicago()(n))
      } else if (palette_name == "Star Trek") {
        return(ggsci::pal_startrek()(n))
      } else if (palette_name == "Tron Legacy") {
        return(ggsci::pal_tron()(n))
      } else if (palette_name == "Futurama") {
        return(ggsci::pal_futurama()(n))
      } else if (palette_name == "Rick and Morty") {
        return(ggsci::pal_rickandmorty()(n))
      } else if (palette_name == "The Simpsons") {
        return(ggsci::pal_simpsons()(n))
      } else if (palette_name == "Flat UI") {
        return(ggsci::pal_flatui()(n))
      } else if (palette_name == "Frontiers") {
        return(ggsci::pal_frontiers()(n))
      } else if (palette_name == "GSEA") {
        return(ggsci::pal_gsea()(n))
      } else if (palette_name == "Bootstrap 5") {
        return(ggsci::pal_bs5()(n))
      } else if (palette_name == "Material Design") {
        return(ggsci::pal_material()(n))
      } else if (palette_name == "Tailwind CSS") {
        return(ggsci::pal_tw3()(n))
      } else {
        return(ggsci::pal_npg()(n))
      }
    }
    
    ########################################################################
    ##### ================= Filter Controls ================= ####
    ########################################################################
    
    ##### ========================= Generate Filter Input for each Data Column (UI) ========================= #####
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
              sliderInput(session$ns(input_id), label = col,
                          min = min(vals, na.rm = TRUE),
                          max = max(vals, na.rm = TRUE),
                          value = range(vals, na.rm = TRUE)),
              switchInput(session$ns(na_id), value = TRUE, onLabel = "include", offLabel = "exclude", size = "mini")
            )
          } else {
            tagList(
              selectizeInput(session$ns(input_id), label = col,
                             choices = c("All", "Has no data"),
                             selected = "All", multiple = TRUE),
              switchInput(session$ns(na_id), value = TRUE, onLabel = "include", offLabel = "exclude", size = "mini")
            )
          }
        })
        do.call(tagList, ui_list)
      })
    })
    
    ##### ========================= Add Choices to Non-Numeric Filters ========================= #####
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
            queryVals <- dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]]
            uniqueVals <- unique(queryVals)
            uniqueVals <- uniqueVals[!is.na(uniqueVals)]
            choices <- c("All", "Has no data", uniqueVals)
            updateSelectizeInput(session, input_id, choices = choices, server = TRUE)
          }
        }
      }
    })
    
    ##### ========================= Function: Filter Data Table (Server) ========================= #####
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
      }) |> debounce(50)
    }
    
    ##### ========================= Apply Filters to All Tables: Intersect GeneIDs ========================= #####
    intersected_gene_ids <- reactive({
      filtered_ids <- lapply(individual_tables, function(tbl) {
        df <- filtered_data(tbl)()
        unique(df$GeneID)
      })
      if (length(filtered_ids) == 0) return(character(0))
      Reduce(intersect, filtered_ids)
    })
    
    ##### ========================= "View Filters" HTML construction ========================= #####
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
    
    ##### ========================= "View filters" (UI) ========================= #####
    observeEvent(input$show_filters, {
      showModal(modalDialog(
        title = "Current Filters",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        uiOutput(session$ns("current_filters"))
      ))
    })
    
    ##### ========================= "Clear filters" ========================= #####
    observeEvent(input$clear_filters, {
      for(tbl in individual_tables) {
        cols <- dbListFields(con, tbl)
        for(col in cols) {
          if(col == "GeneID") next
          input_id <- paste(tbl, col, sep = "_")
          na_id <- paste(input_id, "na", sep = "_")
          sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
          if (is.numeric(sample_val)) {
            vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
            default_val <- range(vals, na.rm = TRUE)
            updateSliderInput(session, input_id, value = default_val)
          } else {
            updateSelectizeInput(session, input_id, selected = "All")
          }
          updateSwitchInput(session, na_id, value = TRUE, onLabel = "include", offLabel = "exclude")
        }
      }
    })
    
    ##### ========================= "Input custom list" (UI) ========================= #####
    observeEvent(input$list_input, {
      showModal(modalDialog(
        title = "Filter custom list (genes, phenotypes etc)",
        fluidPage(
          fluidRow(h5('Select Table and Column to filter')),
          fluidRow(
            column(6,
                   selectInput(session$ns("list_input_table"), "Table:",
                               choices = individual_tables, selected = individual_tables[1])
            ),
            column(6,
                   uiOutput(session$ns("list_input_column_ui"))
            )
          ),
          hr(),
          fluidRow(h5('Input custom list')),
          fluidRow(
            column(12,
                   textInput(session$ns("list_input_text"), NULL, value = "")
            )
          ),
          hr(),
          fluidRow(h5('Select list delimiter')),
          fluidRow(
            column(12,
                   selectInput(session$ns("list_input_separator"), NULL,
                               choices = c("Semicolon (;)" = ";",
                                           "Comma (,)" = ",",
                                           "Whitespace" = " ",
                                           "Pipe (|)" = "|"),
                               selected = ";")
            )
          ),
          fluidRow(
            column(12,
                   htmlOutput(session$ns("list_input_message"))
            )
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          actionButton(session$ns("apply_list_input"), "Apply", class = "btn-primary"),
          modalButton("Close")
        )
      ))
    })
    
    ##### ========================= "Input custom list": Update Column Choices based on Table selected ========================= #####
    output$list_input_column_ui <- renderUI({
      req(input$list_input_table)
      tbl <- input$list_input_table
      cols <- setdiff(dbListFields(con, tbl), "GeneID")
      selectInput(session$ns("list_input_column"), "Select Column:", choices = cols, selected = cols[1])
    })
    
    ##### ========================= "Input custom list" (Server) ========================= #####
    observeEvent(input$apply_list_input, {
      req(input$list_input_table, input$list_input_column, input$list_input_text, input$list_input_separator)
      tbl <- input$list_input_table
      col <- input$list_input_column
      filter_input_id <- paste(tbl, col, sep = "_")
      entries <- unlist(strsplit(input$list_input_text, split = input$list_input_separator, fixed = TRUE))
      entries <- trimws(entries)
      entries <- entries[entries != ""]
      available_values <- as.character(dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]])
      available_values <- available_values[!is.na(available_values)]
      valid_entries <- entries[entries %in% available_values]
      invalid_entries <- setdiff(entries, valid_entries)
      updateSelectizeInput(session, filter_input_id,
                           choices = c("All", "Has no data", available_values),
                           selected = valid_entries)
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
    
    ########################################################################
    ##### ================= Main Panel ================= ####
    ########################################################################
    
    ##### ========================= Render Gene Table ========================= #####
    output$aggregated_table <- DT::renderDataTable({
      req(input$agg_table)
      df <- dbReadTable(con, input$agg_table)
      gene_ids <- intersected_gene_ids()
      if (length(gene_ids) > 0) {
        df <- df[df$GeneID %in% gene_ids, ]
      } else {
        df <- data.frame(Message = "No intersecting GeneIDs found")
      }
      DT::datatable(
        df,
        escape = FALSE,
        filter = "none",
        rownames = FALSE,
        class = "display cell-border stripe",
        options = list(
          paging = TRUE,
          pageLength = 100,
          lengthChange = TRUE,
          searching = TRUE,
          autoWidth = TRUE,
          responsive = TRUE,
          scrollX = TRUE,
          scrollY = "500px",
          # dom = 't<"row"<"col-sm-4"i><"col-sm-4"l><"col-sm-4"p>>',
          dom = 'fltip',
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
    
    ##### ========================= Handle Gene Table Download ========================= #####
    output$download_agg_table <- downloadHandler(
      filename = function() {
        paste0("aggregated_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        gene_ids <- intersected_gene_ids()
        if (length(gene_ids) == 0) {
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
    
    ##### ========================= No Data to Display plot ========================= #####
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
    
    ##### ========================= No Data Selected plot ========================= #####
    selectDataPlot <- function() {
      plot_ly() %>%
        layout(title = "Select data to plot in Plot Options",
               annotations = list(
                 list(text = "Select data to plot in Plot Options",
                      showarrow = FALSE,
                      x = 0.5, y = 0.5,
                      xref = "paper", yref = "paper")
               ))
    }
    
    ##### ========================= Current Plots Dataframe  ========================= #####
    current_plot_df <- reactiveVal(NULL)
    
    ##### ========================= Function: Current Plot being Displayed (Server) ========================= #####
    plot_obj <- reactive({
      req(input$plot_type)
      
      if (input$plot_type == "Bar Chart/Histogram") {
        if (is.null(input$bar_table) || input$bar_table == "" ||
            is.null(input$bar_col) || input$bar_col == "" ||
            is.null(input$bar_gene_lists) || length(input$bar_gene_lists) == 0)
          return(selectDataPlot())
        
        df <- filtered_data(input$bar_table)()
        if (nrow(df) == 0) return(noDataPlot())
        
        gene_ids <- intersected_gene_ids()
        if (length(gene_ids) == 0) return(noDataPlot())
        df <- df[df$GeneID %in% gene_ids, ]
        if (nrow(df) == 0) return(noDataPlot())
        
        combined <- do.call(rbind, lapply(input$bar_gene_lists, function(listName) {
          subset_genes <- if (listName == "Current List") gene_ids else saved_gene_lists$data[[listName]]$genes
          df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
          if (nrow(df_subset) == 0) return(NULL)
          df_subset$gene_list <- listName
          df_subset
        }))
        if (is.null(combined) || nrow(combined) == 0) return(noDataPlot())
        
        if (!is.numeric(combined[[input$bar_col]])) {
          combined$value <- combined[[input$bar_col]]
          if (input$bar_show_na) {
            combined$value <- ifelse(is.na(combined$value), "Missing", combined$value)
          }
          if (input$bar_y_type == "% of genes") {
            agg_df <- combined %>%
              group_by(gene_list, value) %>%
              summarise(count = n_distinct(GeneID), .groups = "drop") %>%
              group_by(gene_list) %>%
              mutate(percentage = count/sum(count)*100) %>%
              ungroup()
            p <- plot_ly(
              data = agg_df,
              x = ~value,
              y = ~percentage,
              color = ~gene_list,
              type = "bar",
              colors = get_palette(input$color_palette, length(unique(agg_df$gene_list)))
            )
          } else {
            agg_df <- combined %>%
              group_by(gene_list, value) %>%
              summarise(count = n_distinct(GeneID), .groups = "drop")
            p <- plot_ly(
              data = agg_df,
              x = ~value,
              y = ~count,
              color = ~gene_list,
              type = "bar",
              colors = get_palette(input$color_palette, length(unique(agg_df$gene_list)))
            )
          }
          current_plot_df(agg_df)
        } else {
          non_missing <- combined[!is.na(combined[[input$bar_col]]), ]
          missing_count <- sum(is.na(combined[[input$bar_col]]))
          bin_args <- if (!is.null(input$bar_bin_size)) list(size = input$bar_bin_size) else NULL
          
          agg_df <- non_missing %>%
            group_by(gene_list) %>%
            do({
              hist_res <- hist(.[[input$bar_col]],
                               breaks = seq(min(.[[input$bar_col]], na.rm = TRUE),
                                            max(.[[input$bar_col]], na.rm = TRUE) + input$bar_bin_size,
                                            by = input$bar_bin_size),
                               plot = FALSE)
              data.frame(bin = hist_res$mids, count = hist_res$counts)
            }) %>% ungroup()
          
          p <- plot_ly()
          if (nrow(non_missing) > 0) {
            p <- add_histogram(p, data = non_missing, x = ~get(input$bar_col),
                               color = ~gene_list,
                               histnorm = ifelse(input$bar_y_type == "% of genes", "percent", ""),
                               xbins = bin_args,
                               colors = get_palette(input$color_palette, length(unique(non_missing$gene_list))))
          }
          if (input$bar_show_na && missing_count > 0) {
            p <- add_trace(p, x = "Missing", y = missing_count, type = "bar", name = "Missing")
          }
          current_plot_df(agg_df)
        }
        
        p <- layout(p,
                    barmode = "group",
                    title = paste("Bar Chart/Histogram of", input$bar_col),
                    xaxis = list(title = input$bar_col),
                    yaxis = list(title = ifelse(input$bar_y_type == "% of genes", "% of genes", "number of genes"))) %>%
          config(displayModeBar = TRUE, displaylogo = FALSE)
        
        thresholdShapes <- list()
        if (!is.na(input$bar_y_threshold1)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$bar_y_threshold1, y1 = input$bar_y_threshold1,
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$bar_y_threshold2)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$bar_y_threshold2, y1 = input$bar_y_threshold2,
            line = list(dash = "dash", color = "red")
          )
        }
        if (length(thresholdShapes) > 0) {
          p <- p %>% layout(shapes = thresholdShapes)
        }
        
        return(p)
        
      } else if (input$plot_type == "Violin/Box Plot") {
        if (is.null(input$violin_table) || input$violin_table == "" ||
            is.null(input$violin_col) || length(input$violin_col) == 0 ||
            is.null(input$violin_gene_lists) || length(input$violin_gene_lists) == 0)
          return(selectDataPlot())
        
        df <- filtered_data(input$violin_table)()
        if (nrow(df) == 0) return(noDataPlot())
        
        gene_ids <- intersected_gene_ids()
        if (length(gene_ids) == 0) return(noDataPlot())
        df <- df[df$GeneID %in% gene_ids, ]
        if (nrow(df) == 0) return(noDataPlot())
        
        combined <- do.call(rbind, lapply(input$violin_gene_lists, function(listName) {
          subset_genes <- if (listName == "Current List") gene_ids else saved_gene_lists$data[[listName]]$genes
          df_subset <- df[df$GeneID %in% subset_genes, , drop = FALSE]
          if (nrow(df_subset) == 0) return(NULL)
          df_subset$gene_list <- listName
          df_subset
        }))
        if (is.null(combined) || nrow(combined) == 0) return(noDataPlot())
        
        # Pivot the data if multiple columns are selected.
        # This creates a long-format data frame with a "variable" column and a "value" column.
        if (length(input$violin_col) > 1) {
          combined_long <- combined %>%
            pivot_longer(
              cols = all_of(input$violin_col),
              names_to = "variable",
              values_to = "value"
            )
        } else {
          combined_long <- combined %>%
            mutate(
              variable = input$violin_col[1],
              value = .[[input$violin_col[1]]]
            )
        }
        
        df_non_missing <- combined_long %>% filter(!is.na(value))
        if (nrow(df_non_missing) == 0) return(noDataPlot())
        
        current_plot_df(df_non_missing)
        
        n_groups <- length(unique(df_non_missing$gene_list))
        
        # Build the plot using the split argument so that data for each selected column is shown in separate traces.
        if (input$violin_plot_style == "violin") {
          p <- plot_ly(
            data = df_non_missing,
            y = ~value,
            type = "violin",
            box = list(visible = FALSE),
            meanline = list(visible = TRUE),
            points = ifelse(input$violin_show_points, "all", "outliers"),
            text = ~paste("Gene:", GeneID, "<br>", variable, ":", value),
            hoverinfo = "text",
            color = ~gene_list,
            split = ~variable,  # This ensures each selected column is split into its own trace.
            colors = get_palette(input$color_palette, n_groups)
          )
        } else if (input$violin_plot_style == "box") {
          p <- plot_ly(
            data = df_non_missing,
            y = ~value,
            type = "box",
            points = ifelse(input$violin_show_points, "all", "outliers"),
            text = ~paste("Gene:", GeneID, "<br>", variable, ":", value),
            hoverinfo = "text",
            color = ~gene_list,
            split = ~variable,
            colors = get_palette(input$color_palette, n_groups)
          )
        } else if (input$violin_plot_style == "violin_box") {
          p <- plot_ly(
            data = df_non_missing,
            y = ~value,
            type = "violin",
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = ifelse(input$violin_show_points, "all", "outliers"),
            text = ~paste("Gene:", GeneID, "<br>", variable, ":", value),
            hoverinfo = "text",
            color = ~gene_list,
            split = ~variable,
            colors = get_palette(input$color_palette, n_groups)
          )
        }
        
        p <- layout(p,
                    title = paste("Violin/Box Plot of", paste(input$violin_col, collapse = ", ")),
                    yaxis = list(title = "Value"))
        
        # Add threshold lines if needed.
        thresholdShapes <- list()
        if (!is.na(input$violin_y_threshold1)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$violin_y_threshold1, y1 = input$violin_y_threshold1,
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$violin_y_threshold2)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$violin_y_threshold2, y1 = input$violin_y_threshold2,
            line = list(dash = "dash", color = "red")
          )
        }
        if (length(thresholdShapes) > 0) {
          p <- p %>% layout(shapes = thresholdShapes)
        }
        
        return(p)
        
      } else if (input$plot_type == "Scatter Plot") {
        if (is.null(input$scatter_table_x) || input$scatter_table_x == "" ||
            is.null(input$scatter_table_y) || input$scatter_table_y == "" ||
            is.null(input$scatter_x) || input$scatter_x == "" ||
            is.null(input$scatter_y) || input$scatter_y == "" ||
            is.null(input$scatter_gene_lists) || length(input$scatter_gene_lists) == 0)
          return(selectDataPlot())
        
        gene_ids <- intersected_gene_ids()
        if (length(gene_ids) == 0) return(noDataPlot())
        
        if (input$scatter_table_x == input$scatter_table_y) {
          df_joint <- filtered_data(input$scatter_table_x)()
          df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
          if (nrow(df_joint) == 0) return(noDataPlot())
          if (!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
            return(noDataPlot())
        } else {
          df_x <- filtered_data(input$scatter_table_x)()
          df_y <- filtered_data(input$scatter_table_y)()
          df_x <- df_x[df_x$GeneID %in% gene_ids, ]
          df_y <- df_y[df_y$GeneID %in% gene_ids, ]
          df_joint <- inner_join(df_x, df_y, by = "GeneID")
          if (nrow(df_joint) == 0) return(noDataPlot())
          if (!(input$scatter_x %in% names(df_joint)) || !(input$scatter_y %in% names(df_joint)))
            return(noDataPlot())
        }
        df_joint <- df_joint[!is.na(df_joint[[input$scatter_x]]) & !is.na(df_joint[[input$scatter_y]]), ]
        
        combined <- do.call(rbind, lapply(input$scatter_gene_lists, function(listName) {
          subset_genes <- if (listName == "Current List") gene_ids else saved_gene_lists$data[[listName]]$genes
          df_subset <- df_joint[df_joint$GeneID %in% subset_genes, , drop = FALSE]
          if (nrow(df_subset) == 0) return(NULL)
          df_subset$gene_list <- listName
          df_subset
        }))
        if (is.null(combined) || nrow(combined) == 0) return(noDataPlot())
        
        gene_map <- gene_mapping()
        combined <- merge(combined, gene_map, by = "GeneID", all.x = TRUE)
        combined$hover_text <- paste("Gene:", combined$symbol)
        
        current_plot_df(combined)
        
        n_groups <- length(unique(combined$gene_list))
        p <- plot_ly(
          data = combined,
          x = ~get(input$scatter_x),
          y = ~get(input$scatter_y),
          type = "scatter",
          mode = "markers",
          color = ~gene_list,
          text = ~hover_text,
          hoverinfo = "text+x+y",
          colors = get_palette(input$color_palette, n_groups)
        )
        
        thresholdShapes <- list()
        if (!is.na(input$scatter_x_threshold1)) {
          thresholdShapes[[length(thresholdShapes)+1]] <- list(
            type = "line",
            x0 = input$scatter_x_threshold1, x1 = input$scatter_x_threshold1,
            y0 = min(combined[[input$scatter_y]], na.rm = TRUE),
            y1 = max(combined[[input$scatter_y]], na.rm = TRUE),
            xref = "x", yref = "y",
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$scatter_x_threshold2)) {
          thresholdShapes[[length(thresholdShapes)+1]] <- list(
            type = "line",
            x0 = input$scatter_x_threshold2, x1 = input$scatter_x_threshold2,
            y0 = min(combined[[input$scatter_y]], na.rm = TRUE),
            y1 = max(combined[[input$scatter_y]], na.rm = TRUE),
            xref = "x", yref = "y",
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$scatter_y_threshold1)) {
          thresholdShapes[[length(thresholdShapes)+1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            y0 = input$scatter_y_threshold1, y1 = input$scatter_y_threshold1,
            yref = "y",
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$scatter_y_threshold2)) {
          thresholdShapes[[length(thresholdShapes)+1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            y0 = input$scatter_y_threshold2, y1 = input$scatter_y_threshold2,
            yref = "y",
            line = list(dash = "dash", color = "red")
          )
        }
        if (length(thresholdShapes) > 0) {
          p <- p %>% layout(shapes = thresholdShapes)
        }
        
        p <- layout(p,
                    title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y),
                    xaxis = list(title = input$scatter_x),
                    yaxis = list(title = input$scatter_y))
        return(p)
        
      } else if (input$plot_type == "Stacked Bar Chart") {
        if (is.null(input$stack_table_x) || input$stack_table_x == "" ||
            is.null(input$stack_x) || input$stack_x == "" ||
            is.null(input$stack_table_y) || input$stack_table_y == "" ||
            is.null(input$stack_y) || input$stack_y == "" ||
            is.null(input$stack_gene_list) || input$stack_gene_list == "")
          return(selectDataPlot())
        
        if (input$stack_table_x == input$stack_table_y) {
          df_joint <- filtered_data(input$stack_table_x)()
        } else {
          df_x <- filtered_data(input$stack_table_x)()
          df_y <- filtered_data(input$stack_table_y)()
          df_joint <- inner_join(df_x, df_y, by = "GeneID")
        }
        if (nrow(df_joint) == 0) return(noDataPlot())
        
        gene_ids <- intersected_gene_ids()
        if (length(gene_ids) == 0) return(noDataPlot())
        df_joint <- df_joint[df_joint$GeneID %in% gene_ids, ]
        if (nrow(df_joint) == 0) return(noDataPlot())
        
        if (input$stack_gene_list != "Current List") {
          subset_genes <- saved_gene_lists$data[[input$stack_gene_list]]$genes
          df_joint <- df_joint[df_joint$GeneID %in% subset_genes, ]
          if (nrow(df_joint) == 0) return(noDataPlot())
        }
        
        xcol <- df_joint[[input$stack_x]]
        ycol <- df_joint[[input$stack_y]]
        
        if (is.numeric(xcol)) {
          bin_size <- if (!is.null(input$stack_bin_size)) input$stack_bin_size else ((max(xcol, na.rm = TRUE) - min(xcol, na.rm = TRUE)) / 50)
          bins <- seq(min(xcol, na.rm = TRUE), max(xcol, na.rm = TRUE) + bin_size, by = bin_size)
          df_joint$bin <- cut(xcol, breaks = bins, include.lowest = TRUE, right = FALSE)
          if (input$stack_show_na_x) {
            df_joint$bin <- as.character(df_joint$bin)
            df_joint$bin[is.na(xcol)] <- "Missing"
          }
        } else {
          df_joint$bin <- as.factor(xcol)
          if (input$stack_show_na_x) {
            df_joint$bin <- as.character(df_joint$bin)
            df_joint$bin[is.na(xcol)] <- "Missing"
          }
        }
        
        df_joint$group <- as.factor(ycol)
        if (input$stack_show_na_y) {
          df_joint$group <- as.character(df_joint$group)
          df_joint$group[is.na(ycol)] <- "Missing"
        }
        
        summary_df <- df_joint %>%
          group_by(bin, group) %>%
          summarise(count = n(), .groups = "drop") %>%
          ungroup()
        if (input$stack_y_type == "% of genes") {
          summary_df <- summary_df %>%
            group_by(bin) %>%
            mutate(percentage = count / sum(count) * 100) %>%
            ungroup()
          y_val <- summary_df$percentage
          y_title <- "% of genes"
        } else {
          y_val <- summary_df$count
          y_title <- "number of genes"
        }
        
        current_plot_df(summary_df)
        
        p <- plot_ly(data = summary_df,
                     x = ~bin,
                     y = ~y_val,
                     color = ~group,
                     type = "bar",
                     colors = get_palette(input$color_palette, length(unique(summary_df$group)))) %>%
          layout(title = paste("Stacked Bar Chart: % of", input$stack_y, "by", input$stack_x),
                 xaxis = list(title = input$stack_x),
                 yaxis = list(title = y_title),
                 barmode = "stack")
        
        thresholdShapes <- list()
        if (!is.na(input$stack_y_threshold1)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$stack_y_threshold1, y1 = input$stack_y_threshold1,
            line = list(dash = "dash", color = "red")
          )
        }
        if (!is.na(input$stack_y_threshold2)) {
          thresholdShapes[[length(thresholdShapes) + 1]] <- list(
            type = "line",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = input$stack_y_threshold2, y1 = input$stack_y_threshold2,
            line = list(dash = "dash", color = "red")
          )
        }
        if (length(thresholdShapes) > 0) {
          p <- p %>% layout(shapes = thresholdShapes)
        }
        
        return(p)
        
      } else if (input$plot_type == "UpSet Plot") {
        req(input$plot_type == "UpSet Plot")
        sets <- gene_list_sets()
        if (length(sets) < 2 || all(sapply(sets, length) < 2)) {
          plot.new()
          text(0.5, 0.5, "Select at least two gene lists to show UpSet Plot.")
          return()
        }
        m <- fromList(sets)
        current_plot_df(sets)
        p <- upset(m, order.by = "freq")
        return(p)
      }
    })
    
    ##### ========================= Display Plot (UI) ========================= #####
    output$plot_ui <- renderUI({
      if (input$plot_type == "UpSet Plot") {
        plotOutput(session$ns("upset_plot"))
      } else {
        card(plotlyOutput(session$ns("plot_output")), full_screen = TRUE, min_height = "600px", max_height = "600px")
      }
    })
    
    ##### ========================= Display Plot (using plot_obj) ========================= #####
    output$plot_output <- renderPlotly({
      plot_obj()
    })
    
    ##### ========================= Display Plot (using plot_obj) ========================= #####
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
    
    ##### ========================= Handle Plot Data Download ========================= #####
    output$download_plot_df <- downloadHandler(
      filename = function() {
        paste0("plot_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- current_plot_df()
        if (!is.null(df) && nrow(df) > 0) {
          write.csv(df, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No plot data available"), file, row.names = FALSE)
        }
      }
    )
    
    ##### ========================= Gene Lists for Upset Plot ========================= #####
    gene_list_sets <- reactive({
      available <- c("Current List", names(saved_gene_lists$data))
      sel <- input$upset_gene_lists
      if (is.null(sel) || length(sel) == 0) sel <- available
      sets <- list()
      if ("Current List" %in% sel)
        sets[["Current List"]] <- intersected_gene_ids()
      for (n in names(saved_gene_lists$data)) {
        if (n %in% sel)
          sets[[n]] <- saved_gene_lists$data[[n]]$genes
      }
      sets
    })
    
    ##### ========================= Generate Upsetplot ========================= #####
    output$upset_plot <- renderPlot({
      req(input$plot_type == "UpSet Plot")
      sets <- gene_list_sets()
      if (length(sets) < 2 || all(sapply(sets, length) < 2)) {
        plot.new()
        text(0.5, 0.5, "Select two lists to show UpSet Plot.")
        return()
      }
      m <- fromList(sets)
      upset(m, order.by = "freq")
    })
  })
}
