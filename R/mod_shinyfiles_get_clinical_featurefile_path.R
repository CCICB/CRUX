#' shinyfiles_get_clinical_featurefile_path UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList 
mod_shinyfiles_get_clinical_featurefile_path_ui <- function(id){
  ns <- NS(id)
  tagList(
        mod_shinyfile_import_ui(id = ns("id_shinyfiles_clinical_featurfile"), title = "Import Metadata", buttonType = "primary", multiple = FALSE, label = "Import Metadata", tooltip_text = "Import Clinical Feature File. If you wish to add tumor-level metadata (e.g. treatment response) but aren't sure how to create the file you need, see Help => FAQ")
  )
}
    
#' Get Path to Clinical Feature File
#' 
#' Server code for getting path to clinical feature file using shinyfiles button.
#' No QC done here. Make sure file is valid when creating MAF object from maf + clinical feature file
#'
#' @inheritParams mod_shinyfile_import_server
#' @inherit mod_shinyfile_import_server return
mod_shinyfiles_get_clinical_featurefile_path_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    maf_path = mod_shinyfile_import_server(id = "id_shinyfiles_clinical_featurfile")
  })
}
    
## To be copied in the UI
# mod_shinyfiles_get_clinical_featurefile_path_ui("shinyfiles_get_clinical_featurefile_path_ui_1")
    
## To be copied in the server
# mod_shinyfiles_get_clinical_featurefile_path_server("shinyfiles_get_clinical_featurefile_path_ui_1")
