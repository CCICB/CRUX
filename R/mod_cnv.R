#' cnv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cnv_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_maf_dataset_wrapper"))
    
  )
}
    
#' cnv Server Functions
#'
#' @noRd 
mod_cnv_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_dataset_wrapper <- mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id ="mod_select_maf_dataset_wrapper", maf_data_pool = maf_data_pool)
    
    
  })
}
    
## To be copied in the UI
# mod_cnv_ui("cnv_ui_1")
    
## To be copied in the server
# mod_cnv_server("cnv_ui_1")
