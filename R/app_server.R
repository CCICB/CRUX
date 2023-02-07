
# Code runs once, result shared for all users -------------------------------------------
# To update default_data_pool.Rds, run / edit update_starting_maf_data_pool()
starting_maf_data_pool <- readRDS(paste0(system.file(package = "CRUX"), "/default_data_pool.Rds"))

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  ### Working on Package
  maf_data_pool <- reactiveVal(starting_maf_data_pool) #starting_maf_data_pool read in from RDS outside server function
  
  mod_home_server("mod_home")
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
  mod_manual_server(id="mod_manual")
  
  #Stop Program when browser is closed
  session$onSessionEnded(function() {
    message("Closing App")
    stopApp()
  })
  
}
