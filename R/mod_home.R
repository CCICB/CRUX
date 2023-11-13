#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinyWidgets::panel(status = "primary", style = 'height: 700px',
        heading = "Getting Started",
        shinyWidgets::actionBttn(inputId = ns('in_bttn_action_publicdata'), label = "Explore Public Datasets", block = TRUE, color = 'royal'),
        tags$br(),
        shinydashboard::box("Explore the genetics of ",tags$strong("57"), " cancer cohorts from the ",tags$strong("TCGA"), " or ",tags$strong("PCAWG")," initiatives.", width = "100%"),
        tags$br(),
        shinyWidgets::actionBttn(inputId = ns('in_bttn_action_virtual_cohorts'), label = "Create Virtual Cohorts", block = TRUE, color = 'success'),
        tags$br(),
        shinydashboard::box("Create virtual cohorts by subsetting public or imported cancer cohorts by ",tags$strong("mutational")," or ", tags$strong("clinical")," features", width = "100%"),
        tags$br(),
        shinyWidgets::actionBttn(inputId = ns('in_bttn_action_userdata'), label = "Explore Your Own Datasets", block = TRUE, color = 'primary'),
        tags$br(),
        shinydashboard::box("Import custom ", tags$strong("whole genome,")," ",tags$strong("whole exome,"),"or ",tags$strong("targeted sequencing")," datasets that describe your cancer type of interest. ", width = "100%"),
        tags$br(),
        shinyWidgets::actionBttn(inputId = ns('in_bttn_action_export'), label = "Export Data", block = TRUE, color = 'warning'),
        tags$br(),
        shinydashboard::box("Export mutational data for use in 3rd party analysis tools, including ",tags$strong("variant annotators, driver gene identifiers,"), "and ",tags$strong("interactive visualisation tools"), width = "100%")
      ) %>% column(width = 4),
    
    shinyWidgets::panel(heading = "Using CRUX", style = "height: 700px", status = "primary",
      #img(src="img/workflows.png", style = "width: auto; height: 650px; display: block; margin: auto", align = "center")
      img(src="img/landing_page_subset.png", style = "width: auto; height: 650px; display: block; margin: auto", align = "center")
    ) %>% column(width = 8)
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$in_bttn_action_publicdata, {
      shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = "DataPool")
      })
    
    observeEvent(input$in_bttn_action_virtual_cohorts, {
      shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = "Subset")
    })
    
    observeEvent(input$in_bttn_action_userdata, {
      shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = "DataImport")
    })
    
    observeEvent(input$in_bttn_action_export, {
      shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = "ExternalTools")
    })
    
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")
