#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_server <- function( input, output, session ) {
  
  starting_maf_data_pool <- new_maf_data_pool()
  
  # prepare TCGA data
  starting_maf_data_pool <- tcga_datasets_to_data_pool(starting_maf_data_pool, source = "Firehose")
  
  # prepare ZCC
  starting_maf_data_pool <- zero_datasets_to_data_pool(starting_maf_data_pool)
  
  # prepare PCAWG datasets
  starting_maf_data_pool <- pcawg_datasets_to_data_pool(starting_maf_data_pool)
    
  ### Working on package
  
  maf_data_pool <- reactiveVal(starting_maf_data_pool)
  
  #mod_data_page_server(id = "mod_data_page", maf_data_pool)
  mod_home_server("mod_home")
  mod_datapool_viewer_server(id = "mod_datapool_viewer", maf_data_pool = maf_data_pool)
  mod_data_import_server(id = "mod_data_import", maf_data_pool = maf_data_pool)
  mod_pan_cohort_statistics_server(id = "mod_pan_cohort_statistics", maf_data_pool)
  mod_compare_cohorts_server(id = "mod_compare_cohorts", maf_data_pool = maf_data_pool)
  moduleEnrichmentAnalysisServer(id = "mod_enrichment_analyis", maf_data_pool)
  mod_survival_analysis_server(id = "mod_survival_analysis", maf_data_pool)
  mod_expression_import_server(id = "mod_expression_import", maf_data_pool)
  mod_expression_analysis_server(id = "mod_expression_analysis", maf_data_pool)
  
  #moduleUtilitiesServer(id = "mod_utilities", maf_data_pool)
  mod_merge_server(id = "mod_merge", maf_data_pool = maf_data_pool)
  moduleSubsetMafsServer(id = "mod_subset", maf_data_pool)
  mod_external_tools_server(id= "mod_external_tools", maf_data_pool)
  mod_sample_level_analysis_server(id = "mod_sample_level_analysis", maf_data_pool)
  mod_cnv_server(id = "mod_cnv_level_analysis", maf_data_pool)
  
  #Stop Program when browser is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
