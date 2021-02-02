# Return List of Mafs -----------------------------------------------------

#' select_datasets_from_maf_data_pool_and_return_maf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_datasets_from_maf_data_pool_and_return_maf_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_select_datasets_from_maf_data_pool_ui(id = ns("mod_get_maf_unique_names"))
  )
}
    

#' mod_select_datasets_from_maf_data_pool_and_return_maf_server Server Functions
#'
#' @description 
#' Similar to mod_select_datasets_from_maf_data_pool_server but returns list of MAF objects
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server
#' @param max_selected_datasets max number of items that can be selected. -1 means unlimited (int)
#' 
#' @returns a list of maf objects or NULL if none were selected (NULL / maf) 
mod_select_datasets_from_maf_data_pool_and_return_maf_server <- function(id, maf_data_pool, max_selected_datasets = -1){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    selected_maf_unique_names <- mod_select_datasets_from_maf_data_pool_server(id = "mod_get_maf_unique_names", maf_data_pool = maf_data_pool, max_selected_datasets = max_selected_datasets)
    
    list_of_mafs = eventReactive(selected_maf_unique_names(), {
      isolate({
        if(is.null(selected_maf_unique_names())) return(NULL) 
        
        lapply(selected_maf_unique_names(), function(unique_name){
          #Package the following four lines plus  a return(loaded_maf) to a module called load_data, apply_changes to maf_data_pool reactiveVal, return loaded maf().
          maf_with_dataset_loaded = maf_data_pool_robust_load(maf_data_pool = maf_data_pool(), unique_name = unique_name)
          maf_data_pool(maf_with_dataset_loaded)
          maf_data_wrapper = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = unique_name)
          loaded_maf = maf_data_wrapper[["loaded_data"]]
          return(loaded_maf)
        })
      })
    })
  })
}



# Return list of maf_dataset_wrappers ---------------------------------------------

#' mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_wrapper_ui <- function(id, label = "select", panel_heading = "Select Dataset"){
  ns <- NS(id)
  tagList(
    mod_select_datasets_from_maf_data_pool_ui(id = ns("mod_get_maf_unique_names"), label = label, panel_heading = panel_heading)
  )
}


#' mod_select_datasets_from_maf_data_pool_and_return_maf_server Server Functions
#'
#' @description 
#' Similar to mod_select_datasets_from_maf_data_pool_server but returns list of MAF dataset wrappers
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server
#' @param max_selected_datasets max number of items that can be selected. -1 means unlimited (int)
#' 
#' @returns a list of maf dataset wrappers or NULL if none were selected (NULL / maf_dataset_wrapper) 
mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_wrapper_server <- function(id, maf_data_pool, max_selected_datasets = -1){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    selected_maf_unique_names <- mod_select_datasets_from_maf_data_pool_server(id = "mod_get_maf_unique_names", maf_data_pool = maf_data_pool, max_selected_datasets = max_selected_datasets)

    list_of_mafs = eventReactive(selected_maf_unique_names(), {
      isolate({
        
        if(is.null(selected_maf_unique_names())) return(NULL) 
        lapply(selected_maf_unique_names(), function(unique_name){
          #Package the following four lines plus  a return(loaded_maf) to a module called load_data, apply_changes to maf_data_pool reactiveVal, return loaded maf().
          maf_with_dataset_loaded = maf_data_pool_robust_load(maf_data_pool = maf_data_pool(), unique_name = unique_name)
          maf_data_pool(maf_with_dataset_loaded)
          maf_data_wrapper = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = unique_name)
          #loaded_maf = maf_data_wrapper[["loaded_data"]]
          return(maf_data_wrapper)
          })
      })
    })
    
    return(list_of_mafs)
  })
}


    
## To be copied in the UI
# mod_select_datasets_from_maf_data_pool_and_return_maf_ui("select_datasets_from_maf_data_pool_and_return_maf_ui_1")
    
## To be copied in the server
# mod_select_datasets_from_maf_data_pool_and_return_maf_server("select_datasets_from_maf_data_pool_and_return_maf_ui_1")
