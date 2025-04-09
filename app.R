##### ========================= Libraries ========================= #####
library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(DT)
library(plotly)
library(dplyr)
library(arrow)
library(zip)
library(UpSetR)
library(shinyWidgets)
library(shinycssloaders)
library(tidyr)

# Source Modules
source("./modules/main_module.R")
main <- "main_mod"

source("./modules/goORA_module.R")
goORA <- "goORA"

source("./modules/reactomeORA_module.R")
reactomeORA <- "reactomeORA"

source("./modules/oddsRatio_module.R")
oddsRatio <- "oddsRatio"

##### ========================= duckDB connect and access Tables ========================= #####
con <- dbConnect(duckdb(), "mydb.duckdb")
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "aggregated")

##### ========================= User saved gene lists ========================= #####
saved_gene_lists <- reactiveValues(data = list())

######################################################################
####### ========================= UI ========================= #######
######################################################################
ui <- page_navbar(
  id = "main_nav",                # Set ID for programmatic updates
  title = "EGEx",
  padding = "0.4rem",
  theme = bs_theme(bootswatch = "cosmo"),
  ##### ========================= Custom CSS ========================= #####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  ##### ========================= Home Page ========================= #####
  nav_panel("Home",
            fluidPage(
              h3("Welcome to EGEx"),
              fluidRow(
                column(6, actionButton("go_explore", "Go to Explore Data", class = "btn-primary")),
                column(6, actionButton("go_overrep", "Go to Over-representation Analysis", class = "btn-primary"))
              )
            )
  ),
  ##### ========================= Explore Data Page ========================= #####
  nav_panel("Explore Data",
            mainModuleUI(main, all_tables, individual_tables)
  ),
  ##### ========================= Over-representation Analysis ========================= #####
  nav_panel("Over-representation Analysis",
            tabsetPanel(
              tabPanel("Gene Ontology",
                       goORAUI(goORA, saved_gene_lists)
              ),
              tabPanel("Reactome",
                       reactomeORAUI(reactomeORA, saved_gene_lists)
              ),
              tabPanel("OddsRatio",
                       oddsRatioUI(oddsRatio, saved_gene_lists)
              )
            )
  ),
  ##### ========================= Explore Data Page ========================= #####
  nav_panel("Downloads",

  )

)

######################################################################
##### ========================= Server ========================= #####
######################################################################
server <- function(input, output, session) {

  # Call module server functions
  mainModuleServer(main,  con, individual_tables, saved_gene_lists)
  goORAServer(goORA, con, saved_gene_lists)
  reactomeORAServer(reactomeORA, con, saved_gene_lists)
  oddsRatioServer(oddsRatio, con, saved_gene_lists)

  # Navigation Observers:
  # When the "Go to Explore Data" button is clicked, update the navbar to "Explore Data"
  observeEvent(input$go_explore, {
    updateNavbarPage(session, "main_nav", selected = "Explore Data")
  })

  # When the "Go to Over-representation Analysis" button is clicked,
  # update the navbar to "Over-representation Analysis"
  observeEvent(input$go_overrep, {
    updateNavbarPage(session, "main_nav", selected = "Over-representation Analysis")
  })
}

shinyApp(ui, server)
