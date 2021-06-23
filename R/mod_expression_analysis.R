#' expression_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_expression_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
  
    shinyWidgets::panel("Expression data is of limited use in this version of CRUX. Currently, you can't do much more than export the data for use in the Xena Browser. Features will be added in future versions."),
    # Step 1: Select Dataset --------------------------------------------------
    mod_select_maf_dataset_wrapper_ui(ns("mod_select_dataset")),
    
    # Step 2: Visualise Data --------------------------------------------------
    shinyWidgets::panel(
      heading = "Tabular Form",
      mod_render_downloadabledataframe_ui(ns("mod_dt_expression")),
    )
    
    #mod_render_downloadabledataframe_ui(ns("mod_dt_mut")),
    
    #textOutput(ns("tmp"))
  )
}
    
#' expression_analysis Server Functions
#'
#' @noRd 
mod_expression_analysis_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # Step 1: Select Dataset --------------------------------------------------
    maf_dataset_wrapper <- mod_select_maf_dataset_wrapper_server("mod_select_dataset", maf_data_pool = maf_data_pool, label = "Step 1: Select Dataset")
    
    maf <- reactive({
      maf_dataset_wrapper()$loaded_data
      })
    
    maf_df <- reactive({
      message("selected ", maf_dataset_wrapper()$unique_name)
      print(maf())
      a=maf() %>% maftools_get_all_data()
      #message("YO")
      return(a)
      })
    
    #output$tmp <- renderText("This is a test")
    expression_df <- reactive({
      validate(need(maf_dataset_wrapper(), message = "Loading data ..."))
      validate(need(maf_dataset_wrapper()$status == "ready", message = "Waiting for data to load ..."))
      maf_data_wrapper_get_rnaseq_data_for_samples_with_mutation_data(maf_dataset_wrapper())
      })
    
    
    # Step 2: Visualise Data --------------------------------------------------
    
    mod_render_downloadabledataframe_server("mod_dt_expression", tabular_data_object = expression_df, basename = "expression_data", message_if_tabular_data_is_null = "No Expression Data Found")
    
    # mod_render_downloadabledataframe_server("mod_dt_mut", tabular_data_object = maf_df, basename = "hello")
  })
}
    
## To be copied in the UI
# mod_expression_analysis_ui("expression_analysis_ui_1")
    
## To be copied in the server
# mod_expression_analysis_server("expression_analysis_ui_1")
