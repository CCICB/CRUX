#' shinyfiles_get_maf_path UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_shinyfiles_get_maf_path_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(style = "width: 100%",
        mod_shinyfile_import_ui(id = ns("id_shinyfiles_maf"), title = "Import MAF", multiple = FALSE, label = "Import MAF", buttonType = "primary", 
                                tooltip_text = "Import a MAF file. If you are not familiar with this filetype, see help => faq"),
    )
  )
}
    
#' shinyfiles_get_maf_path Server Functions
#'
#' @inheritParams mod_shinyfile_import_server
#' @inherit mod_shinyfile_import_server return
mod_shinyfiles_get_maf_path_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    maf_path = mod_shinyfile_import_server(id = "id_shinyfiles_maf")
  })
}

## To be copied in the UI
# mod_shinyfiles_get_maf_path_ui("shinyfiles_get_maf_path_ui_1")

## To be copied in the server
# mod_shinyfiles_get_maf_path_server("shinyfiles_get_maf_path_ui_1")
