#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @export
app_server <- function( input, output, session ) {
  starting_maf_data_pool <- new_maf_data_pool()
  starting_maf_data_pool <- tcga_datasets_to_data_pool(starting_maf_data_pool)
  
  
  #starting_maf_data_pool <- pcawg_datasets_to_data_pool(starting_maf_data_pool)
  #browser()
  maf_data_pool <- reactiveVal(starting_maf_data_pool)
  # List the first level callModules here
  
  mod_data_page_server(id = "mod_data_page", maf_data_pool)
  mod_pan_cohort_statistics_server(id = "mod_pan_cohort_statistics", maf_data_pool)
  mod_compare_cohorts_server(id = "mod_compare_cohorts", maf_data_pool = maf_data_pool)
  moduleEnrichmentAnalysisServer(id="mod_enrichment_analyis", maf_data_pool)
  moduleUtilitiesServer(id = "mod_utilities", maf_data_pool)
  mod_external_tools_server(id= "mod_external_tools", maf_data_pool)
  
  #Stop Program when browser is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  #maf = reactive({maftools::read.maf(system.file("test_data/tcga_laml.subsampled.maf.gz", package = "shinymaftools"))})
  #mod_utilities_create_clinical_data_spreadsheet_server(id = "mod_create_clinical_data_spreadsheet", maf = maf)
  
  # debug -------------------------------------------------------------------
}
