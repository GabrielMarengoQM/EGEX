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
source("./modules/downloads_module.R")
downloads <- "downloads"


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
            downloadsUI(downloads)
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
  downloadsServer(downloads, con, saved_gene_lists)
}

shinyApp(ui, server)
