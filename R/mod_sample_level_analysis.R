#' sample_level_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sample_level_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    shinyWidgets::panel(
      heading="Step 1: Select Cohort",
      mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_dataset"), panel = FALSE)
    ),
    
    shinyWidgets::panel(
      heading="Step 2: Select Sample",
      mod_select_tumor_sample_barcode_from_maf_ui(id = ns("mod_mod_select_tumor_sample_barcode"))
      #mod_select_tumor_sample_from_maf_datapool()
    ),
    
    
    shinyWidgets::panel(
      heading = "Step 3: Choose Analysis",
      tabsetPanel(
        tabPanel(title = "Rainfall", mod_plot_rainfall_ui(ns("mod_plot_rainfall"))),
        tabPanel(title = "Clonal Heterogeneity", mod_plot_heterogeneity_ui(id=ns("mod_plot_heterogeneity")))
      )
    )
  )
}

#' sample_level_analysis Server Functions
#'
#' @noRd 
mod_sample_level_analysis_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Select Data -------------------------------------------------------------
    maf_dataset_wrappper_unvalidated <- mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id = "mod_select_dataset", maf_data_pool = maf_data_pool)
    observe({ maf_dataset_wrappper_unvalidated()})
    
    maf_dataset_wrappper <- reactive({
      validate(need(!is.null(maf_dataset_wrappper_unvalidated()), message = "Loading data .."))
      maf_dataset_wrappper_unvalidated()
      })
    
    maf <- reactive({ maf_dataset_wrappper()$loaded_data })
    tsb <- mod_select_tumor_sample_barcode_from_maf_server(id = "mod_mod_select_tumor_sample_barcode", maf = maf)
    
    
    # Modules ------------------------------------------------------------------
    mod_plot_rainfall_server(id = "mod_plot_rainfall", maf=maf, tsb=tsb)
    mod_plot_heterogeneity_server(id = "mod_plot_heterogeneity", maf =maf, tsb=tsb)
  })
}

## To be copied in the UI
# mod_sample_level_analysis_ui("sample_level_analysis_ui_1")

## To be copied in the server
# mod_sample_level_analysis_server("sample_level_analysis_ui_1")
