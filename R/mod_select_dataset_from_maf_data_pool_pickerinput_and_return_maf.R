
# Single File -------------------------------------------------------------
#' mod_select_dataset_from_data_pool_dropdown_and_return_maf_
#'
#' @description UI for selecting a dataset from the maf_data_pool.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param panel should pickerinput be wrapped in a shinyWidgets::panel (flag)
#'
#' @importFrom shiny NS tagList 
mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui <- function(id, panel=TRUE){
  ns <- NS(id)
  tagList(
    mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset"), panel=panel),
  )
}
    
#' smod_select_dataset_from_data_pool_dropdown_and_return_maf_ Server Functions
#' @param maf_data_pool the maf_data_pool (maf_data_pool; reactiveVal)
#'
#' @description wraps mod_select_dataset_from_maf_data_pool_pickerinput_server.
#' Instead of returning a unique_name, it returns a maf object, having loaded the dataset into memory if required and updated maf_data_pool to indicate as much
#'
#' @return selected maf object or NULL if none is selected (maf; reactive)
mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server <- function(id, maf_data_pool, label = "Dataset"){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    #browser()
    ns <- session$ns
    selected_dataset_unique_name <- reactive({
      mod_select_dataset_from_maf_data_pool_pickerinput_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool, label=label)()
    })
    
    #outputOptions(output, "in_picker_dataset", suspendWhenHidden = FALSE)
    
    maf <- reactive({
      validate(need(!is.null(selected_dataset_unique_name()), message = "Please select a dataset"))
      new_data_pool <- maf_data_pool_robust_load(isolate(maf_data_pool()), selected_dataset_unique_name())
      isolate(maf_data_pool(new_data_pool))
      maf_data_pool_unique_name_to_maf_nonreactive(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
    })
    
  })
}


# Multiple Files ----------------------------------------------------------
mod_select_datasets_from_maf_data_pool_pickerinput_and_return_maf_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset")),
    #dipsaus::actionButtonStyled(inputId = ns("in_button_run_action"), label = "Select")
  )
}


mod_select_datasets_from_maf_data_pool_pickerinput_and_return_maf_server <- function(id, maf_data_pool, trigger_for_file_load){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  utilitybeltshiny::assert_reactive(trigger_for_file_load)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$selected_dataset_unique_names <- reactive({
      mod_select_dataset_from_maf_data_pool_pickerinput_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool, multiple = TRUE)()
    })
    
    outputOptions(output, "selected_dataset_unique_names", suspendWhenHidden = FALSE)
    
    #Only load files into memory when 'trigger_for_file_load' changes
    eventReactive( trigger_for_file_load(), {
      #browser()
      isolate({
        validate(need(!is.null(selected_dataset_unique_names()), message = "Please select a dataset"))
        list_of_maf_objects = purrr::map(.x = selected_dataset_unique_names(), .f = function(unique_name) {
          new_data_pool <- maf_data_pool_robust_load(isolate(maf_data_pool()), selected_dataset_unique_name())  
          isolate(maf_data_pool(new_data_pool))
          maf = maf_data_pool_unique_name_to_maf_nonreactive(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
        })
        
        return(list_of_maf_objects)
      })
    })
    
    # maf_list <- reactive({
    # 
    # })
  })
}
    
## To be copied in the UI
# mod_select_dataset_from_dropdown_ui("select_dataset_from_dropdown_ui_1")
    
## To be copied in the server
# mod_select_dataset_from_dropdown_server("select_dataset_from_dropdown_ui_1")
