#' import_clinical_featurefile UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList 
mod_import_clinical_featurefile_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_shinyfile_import_ui(ns("mod_shinyfiles_get_path_to_metadata_df"), title = "Import Clinical Feature File", label = "Import Clinical Feature File", multiple = FALSE),
    DT::dataTableOutput(ns("out_dt_clinical_data")) %>% shinycssloaders::withSpinner(proxy.height = "200px")
  )
}
    
#' import_clinical_featurefile Server Functions
#' 
#' @description  Imports a clinical feature file. 
#' @param id Internal parameters for {shiny}.
#' @param maf_path path to maf file (reactive string)
#' @return  the object with metadata from a clinical feature
#' 
mod_import_clinical_featurefile_server <- function(id, maf_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    path = mod_shinyfile_import_server("mod_shinyfiles_get_path_to_metadata_df")
    
    maf_with_clinical_data <- reactive({
      #browser()
      validate(need(!is.null(maf_path()), message = "Please Import a MAF file"))
      validate(need(!is.null(path()) && assertthat::is.string(path()) && length(path()) > 0, message =  "Please Import a Metadata File"))
      message("PATH: ", path())
      #browser()
      #validate(need(is_valid_clinicalfeaturefile(path(), maf_path()), label = paste0("Either MAF or Clinical Feature File is invalid: error\n", is_valid_clinicalfeaturefile_return_error(path(), maf()))))
      #browser()
      validate(need(is_valid_clinicalfeaturefile(NULL, maf_path()), label = paste0("MAF file is invalid. ERROR: ", is_valid_clinicalfeaturefile_return_error(NULL, maf_path()))))
      validate(need(is_valid_clinicalfeaturefile(path(), maf_path()), label = paste0("Clinical Feature File is invalid: ", is_valid_clinicalfeaturefile_return_error(path(), maf_path()))))
      #browser()
      return(maftools::read.maf(maf_path(), path()))
    })
    
   output$out_dt_clinical_data <- DT::renderDataTable({
     #browser()
     validate(need(!is.null(maf()), message = "Please import a MAF file"))
     validate(need(!is.null(maf_with_clinical_data()), message = "Please import Clinical Feature File"))
     #validate(need(!is.null(maf_with_clinical_data), message = "Please import a Clinical Data file"))
     maftools::getClinicalData(maf_with_clinical_data())
     }, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
   
     
    return(maf_with_clinical_data)
  })
}


    
## To be copied in the UI
# mod_import_clinical_featurefile_ui("import_clinical_featurefile_ui_1")
    
## To be copied in the server
# mod_import_clinical_featurefile_server("import_clinical_featurefile_ui_1")
