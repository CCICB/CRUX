#' select_maf_column_value UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_maf_column_value_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' select_maf_column_value Server Functions
#'
#' @noRd 
mod_select_maf_column_value_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_select_maf_column_value_ui("select_maf_column_value_ui_1")
    
## To be copied in the server
# mod_select_maf_column_value_server("select_maf_column_value_ui_1")
