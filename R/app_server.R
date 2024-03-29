# Code runs once, result shared for all users -------------------------------------------

# To update default_data_pool.Rds, run / edit update_starting_maf_data_pool()
path_to_default_data_pool <- function(){
  system.file(package = "CRUX", "default_data_pool.Rds")
}

message('Testing whether system.file works: [', system.file(package = "CRUX"), ']')

if(nchar(path_to_default_data_pool() > 0)){
  message('Loading CRUX Starting Datapool from: ', path_to_default_data_pool())
  
  starting_maf_data_pool <- readRDS(path_to_default_data_pool())
}else{ 
  starting_maf_data_pool <- NULL
}


external_tool_metadata <- external_tools_load_all_tools()


# Server Options
options(shiny.maxRequestSize=50*1024^2) # Increase upload filesize limit from 5 to 50mb


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @include fct_external_tools_load_dataframe.R
#' @noRd
app_server <- function( input, output, session ) {
  
  ### Working on Package
  maf_data_pool <- reactiveVal(starting_maf_data_pool) #starting_maf_data_pool read in from RDS outside server function
  
  mod_home_server("mod_home", parent_session = session)
  mod_datapool_viewer_server(id = "mod_datapool_viewer", maf_data_pool = maf_data_pool, parent_session = session)
  mod_data_import_server(id = "mod_data_import", maf_data_pool = maf_data_pool)
  mod_pan_cohort_statistics_server(id = "mod_pan_cohort_statistics", maf_data_pool)
  mod_compare_cohorts_server(id = "mod_compare_cohorts", maf_data_pool = maf_data_pool)
  mod_mutational_signatures_server(id = "mod_mutational_signatures", maf_data_pool)
  moduleEnrichmentAnalysisServer(id = "mod_enrichment_analyis", maf_data_pool)
  mod_survival_analysis_server(id = "mod_survival_analysis", maf_data_pool)
  #mod_expression_import_server(id = "mod_expression_import", maf_data_pool)
  #mod_expression_analysis_server(id = "mod_expression_analysis", maf_data_pool)
  
  # Utilities
  mod_merge_server(id = "mod_merge", maf_data_pool = maf_data_pool)
  mod_utility_subset_server(id = "mod_subset", maf_data_pool)
  mod_external_tools_server(id= "mod_external_tools", maf_data_pool)
  mod_sample_level_analysis_server(id = "mod_sample_level_analysis", maf_data_pool)
  mod_cnv_server(id = "mod_cnv_level_analysis", maf_data_pool)
  
  
  # Manual
  mod_help_server(id="mod_help")
  mod_privacy_server(id = "mod_privacy")
  
  #Stop Program when browser is closed
  session$onSessionEnded(function() {
    message("Closing App")
    stopApp()
  })
  
}
