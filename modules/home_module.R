##### ========================= Home Module UI ========================= #####
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Welcome to EGEx"),
      fluidRow(
        column(6,
               actionButton(ns("go_explore"), "Go to Explore Data", 
                            class = "btn btn-primary btn-lg", width = "100%")
        ),
        column(6,
               actionButton(ns("go_overrep"), "Go to Over-representation Analysis", 
                            class = "btn btn-primary btn-lg", width = "100%")
        )
      )
    ),
    br(),
    h5("Data Sources"),
    hr(),
    h5("Publications"),
    hr(),
    h5("GitHub"),
    hr(),
    h5("Disclaimer")
  )
}

##### ========================= Home Module Server ========================= #####
homeServer <- function(id, nav_id) {
  moduleServer(id, function(input, output, session) {
    
    # Use update_page_navbar() from bslib to update the active nav panel.
    # Make sure you have a recent version of bslib installed.
    observeEvent(input$go_explore, {
      updateNavbarPage(session, inputId = nav_id, selected = "Explore Data")
    })
    
    observeEvent(input$go_overrep, {
      updateNavbarPage(session, inputId = nav_id, selected = "Over-representation Analysis")
    })

  })
}