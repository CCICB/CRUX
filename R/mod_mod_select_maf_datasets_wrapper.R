#' mod_select_maf_datasets_wrapper UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_select_maf_datasets_wrapper_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::multiInput(
      inputId = ns("in_multi_input"),
    choices = character(0),
    selected = character(0),
    label = "Select datasets to merge", width = "100%"
    ),
    shiny::actionButton(inputId = ns("in_bttn_merge"), label = "Merge", width = "100%")
  )
}
    
#' mod_select_maf_datasets_wrapper Server Functions
#'
#' @return a list of maf_dataset_wrappers, or NULL if none are selected
#'
#' @noRd 
mod_mod_select_maf_datasets_wrapper_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    prev_unique_dataset_names <- reactiveVal(NULL)
    
    # Will run every time maf_data_pool changes (even if a maf_dataset_wrapper$loaded_data slot changes),
    # but will only update the cohort list if a new cohort is added
    
    observeEvent(maf_data_pool(), isolate({
      validate(need(!is.null(maf_data_pool()), message = "Loading ... "))
      maf_data_pool_df <- maf_data_pool_to_dataframe(maf_data_pool())
      unique_dataset_names <- maf_data_pool_df[["unique_name"]]
      
      # Check new cohort added
      if(!identical(unique_dataset_names, prev_unique_dataset_names())){
        prev_unique_dataset_names(unique_dataset_names)
        
        sample_numbers <- sample_numbers_formatted(maf_data_pool_df)
        display_names <- display_names_formatted(maf_data_pool_df)
        short_dataset_names <- short_dataset_names_badge(maf_data_pool_df)
        data_sources <- data_sources_formatted(maf_data_pool_df)
        
        content <- paste0(
          display_names,
          " ",
          sample_numbers,
          short_dataset_names,
          data_sources
        )
      
        names(unique_dataset_names) <- content
        
        shinyWidgets::updateMultiInput(session, inputId = "in_multi_input", choices = unique_dataset_names)
      }
      }))
    
    #Return list of selected maf_dataset_wrappers
    maf_dataset_wrappers <- eventReactive(
    eventExpr = input[["in_bttn_merge"]],
    valueExpr = isolate({
      selected_dataset_unique_names <- input[["in_multi_input"]]
      
      if(length(selected_dataset_unique_names) < 2){
        message(selected_dataset_unique_names)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Select a Dataset",
          text = "You must select at least 2 datasets to merge",
          type = "error"
        )
        return(NULL)
      }
      #Step 0: Validate
      validate(need(!is.null(selected_dataset_unique_names), message = "Please Select a Dataset"))

      # Step 1: Grab dataset wrappers as a list
      ls_maf_data_wrappers <- lapply(selected_dataset_unique_names, FUN = function(unique_name){
        
        # Step 1a: Load dataset wrapper and update maf data pool
        maf_data_pool_with_dataset_loaded = maf_data_pool_robust_load(maf_data_pool = maf_data_pool(), unique_name = unique_name)
        maf_data_pool(maf_data_pool_with_dataset_loaded)
        
        # Step 1b: get maf_dataset_wrapper and return
        maf_data_wrapper = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = unique_name)
        return(maf_data_wrapper)
      })

      # Step 2: Grab Data Wrapper
      return(ls_maf_data_wrappers)
     }))
    
    return(maf_dataset_wrappers)
  })
}
    
## To be copied in the UI
# mod_mod_select_maf_datasets_wrapper_ui("mod_select_maf_datasets_wrapper_1")
    
## To be copied in the server
# mod_mod_select_maf_datasets_wrapper_server("mod_select_maf_datasets_wrapper_1")
