#' select_dataset_from_maf_data_pool_pickerinput_and_return_maf_and_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui
#'
#' @importFrom shiny NS tagList 
mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui <- function(id, panel=TRUE){
  ns <- NS(id)
  tagList(
    mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset"), panel=panel),
  )
}

#' Select Dataset, Return maf_dataset_wrapper
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_server
#' 
#' @description wraps mod_select_dataset_from_maf_data_pool_pickerinput_server.
#' Instead of simply returning a unique_name, this function will:
#' 
#' 
#' \enumerate{ 
#' \item Load the specified dataset into memory if required.
#' \item Update maf_data_pool (a reactiveVal) to indicate the dataset has been loaded.
#' \item Return the relevant maf_dataset_wrapper. See \strong{Accessing Properties} section for details.
#' }
#' 
#' @section Accessing Properties (Quick Reference):
#' 
#' \tabular{rrr}{
#' \strong{MAF object:} \tab \tab \code{maf_dataset_wrapper()$loaded_data} \cr
#' \strong{unique name:} \tab \tab \code{maf_dataset_wrapper()$unique_name} \cr
#' \strong{short name:} \tab \tab \code{maf_dataset_wrapper()$short_name} \cr
#' \strong{full name:} \tab \tab \code{maf_dataset_wrapper()$display_name} \cr 
#' }
#' 
#' 
#' See ?new_maf_dataset_wrapper for the full list of properties
#' 
#' 
#' 
#' @return maf_dataset_wrapper.
mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server <- function(id, maf_data_pool, label = "Dataset"){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    #browser()
    ns <- session$ns
    selected_dataset_unique_name <- reactive({
      mod_select_dataset_from_maf_data_pool_pickerinput_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool, label=label)()
    })
    
    #outputOptions(output, "in_picker_dataset", suspendWhenHidden = FALSE)
    
    # maf <- reactive({
    #   validate(need(!is.null(selected_dataset_unique_name()), message = "Please select a dataset"))
    #   new_data_pool <- maf_data_pool_robust_load(isolate(maf_data_pool()), selected_dataset_unique_name())
    #   isolate(maf_data_pool(new_data_pool))
    #   maf_data_pool_unique_name_to_maf_nonreactive(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
    # })
    
    maf_dataset_wrapper <- eventReactive(selected_dataset_unique_name(), { #Run only when pickerinput selected dataset changes changes
      isolate({
        #Step 0: Validate 
        validate(need(!is.null(selected_dataset_unique_name()), message = "Please Select a Dataset"))
        
        #Step 1: Load data and update maf_data_pool
        new_data_pool <- maf_data_pool_robust_load(maf_data_pool(), selected_dataset_unique_name())
        maf_data_pool(new_data_pool)
        
        #Step 2: Grab Data Wrapper
        maf_dataset_wrapper_ = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
        return(maf_dataset_wrapper_)
      })
    })
    
    return(maf_dataset_wrapper)
    
  })
}
    
## To be copied in the UI
# mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")
    
## To be copied in the server
# mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")
