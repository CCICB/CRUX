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
      #heading="Step 1: Select Dataset",
      heading="Step 1: Select Dataset",
      mod_select_maf_dataset_wrapper_ui(ns("in_picker_dataset"), panel = FALSE),
      # mod_select_maf_dataset_wrapper_server(ns("mod_select_dataset"))
    ), icon_down_arrow(break_after = TRUE),
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

    maf_dataset_wrapper<- mod_select_maf_dataset_wrapper_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool)
    
    selected_dataset_unique_name <- reactive({
      maf_dataset_wrapper()$unique_name
      })
    
    single_cohort_maf <- reactive({
      validate(need(!is.null(selected_dataset_unique_name()), message = "Please select a dataset"))
        maf_dataset_wrapper()$loaded_data
      })


    cohort_name <- reactive({
      validate(need(!is.null(maf_dataset_wrapper()), message = "Please select a dataset"))
      return(maf_dataset_wrapper()$display_name)

    })
    # 
    
    mod_single_cohort_summary_tables_and_plots_server(id = "tables_and_plots", maf = single_cohort_maf, cohortName = cohort_name)
  })
}
    
## To be copied in the UI
# mod_pan_cohort_statistics_ui("pan_cohort_statistics_ui_1")
    
## To be copied in the server
# mod_pan_cohort_statistics_server("pan_cohort_statistics_ui_1")
