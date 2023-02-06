#' edit_dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_edit_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_select_datasets_from_maf_data_pool_ui(id = ns("mod_select_dataset")),
    mod_select_dataset_from_maf_data_pool_pickerinput_ui(id=ns("mod_select_dataset_pickerinput"))
  )
}
    
#' edit_dataset Server Functions
#'
#' @noRd 
mod_edit_dataset_server <- function(id, maf_data_pool){
  assertions::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    mod_select_datasets_from_maf_data_pool_server(id = "mod_select_dataset", maf_data_pool = maf_data_pool)
    #mod_select_dataset_from_maf_data_pool_pickerinput_server(id="mod_select_dataset_pickerinput", maf_data_pool = maf_data_pool)
  })
}
    
## To be copied in the UI
# mod_edit_dataset_ui("edit_dataset_ui_1")
    
## To be copied in the server
# mod_edit_dataset_server("edit_dataset_ui_1")
