#' pan_cohort_statistics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pan_cohort_statistics_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    shinyWidgets::panel(
      heading="Step 1: Select Dataset",
      mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset"), panel = FALSE)
    ),
    mod_single_cohort_summary_tables_and_plots_ui(id = ns("tables_and_plots"))
  )
}
    
#' pan_cohort_statistics Server Functions
#'
#' @noRd 
mod_pan_cohort_statistics_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(maf_data_pool(), message("MAF_DATAPOOL_JUST_CHANGED"))
    
    
    selected_dataset_unique_name <- reactive({
      mod_select_dataset_from_maf_data_pool_pickerinput_server(id = "in_picker_dataset", maf_data_pool = isolate(maf_data_pool))()
      })

    single_cohort_maf <- reactive({
      validate(need(!is.null(selected_dataset_unique_name()), message = "Please select a dataset"))
        new_data_pool <- maf_data_pool_robust_load(isolate(maf_data_pool()), selected_dataset_unique_name())
        isolate(maf_data_pool(new_data_pool))
        maf_data_pool_unique_name_to_maf_nonreactive(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
      })


    cohort_name <- reactive({
      validate(need(!is.null(selected_dataset_unique_name()), message = "Please select a dataset"))
      maf_data_wrapper = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), selected_dataset_unique_name())
      return(maf_data_wrapper[["display_name"]])

    })
    
    
    mod_single_cohort_summary_tables_and_plots_server(id = "tables_and_plots", maf = single_cohort_maf, cohortName = cohort_name)
    #unique_name to maf object ()
  })
}
    
## To be copied in the UI
# mod_pan_cohort_statistics_ui("pan_cohort_statistics_ui_1")
    
## To be copied in the server
# mod_pan_cohort_statistics_server("pan_cohort_statistics_ui_1")
