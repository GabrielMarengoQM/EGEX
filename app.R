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
library(future)
plan(multisession) # set up a future

# Source Modules
source("./modules/main_module.R")
main <- "something"

source("./modules/goORA_module.R")
goORA <- "goORA"

source("./modules/reactomeORA_module.R")
reactomeORA <- "reactomeORA"

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
  title = "EGEx",
  padding = "0.4rem",
  theme = bs_theme(bootswatch = "cosmo"),
  ##### ========================= Custom CSS ========================= #####
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
   /* Style the Plotly modebar */
    .modebar {
      background-color: #f0f0f0 !important;  /* light grey background */
      border: none !important;               /* no border */
      border-radius: 4px !important;         /* keeps rounded corners if desired */
      position: absolute;
      top: 0;
      right: 0;
      padding: 0 0 4px 4px;                   /* no padding on top/right, some on bottom/left */
    }

    "))
  ),
  ##### ========================= Explore Data Page ========================= #####
  nav_panel("Explore Data",
            mainModuleUI(main, all_tables, individual_tables)
  ),
  nav_panel("Over-representation Analysis",
            tabsetPanel(
              tabPanel("Gene Ontology",
                       goORAUI(goORA, saved_gene_lists)
              ),
              tabPanel("Reactome",
                       reactomeORAUI(reactomeORA, saved_gene_lists)

              ),
              tabPanel("Custom",

              )
            )
            )
)

######################################################################
##### ========================= Server ========================= #####
######################################################################
server <- function(input, output, session) {

  mainModuleServer(main,  con, individual_tables, saved_gene_lists)
  goORAServer(goORA, con, saved_gene_lists)
  reactomeORAServer(reactomeORA, con, saved_gene_lists)
}

shinyApp(ui, server)
