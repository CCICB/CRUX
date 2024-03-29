#' render_clinical_data_table UI Function
#'
#' @description A shiny Module. Renders clinica data attached to a maf object in the form of a datatable  
#'
#' @param id Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList 
mod_render_clinical_data_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("clinical_feature_table"))
  )
}
    

#' Title
#'
#' @param maf maf object (reactive)
#' @param id Internal parameters for {shiny}.
#' @return Nothing. Run for its side effects
#'
#'
mod_render_clinical_data_table_server <- function(id, maf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    assertions::assert_reactive(maf)
    
    clinical_data_df <- reactive({
      validate(need(!is.null(maf()), message = "Please import a valid maf / tumor-level metadata file"))
      maftools::getClinicalData(maf()) %>% return()
    })
    
    output$clinical_feature_table <- DT::renderDataTable({ clinical_data_df()}, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
  })
}
    
## To be copied in the UI
# mod_render_clinical_data_table_ui("render_clinical_data_table_ui_1")
    
## To be copied in the server
# mod_render_clinical_data_table_server("render_clinical_data_table_ui_1")
