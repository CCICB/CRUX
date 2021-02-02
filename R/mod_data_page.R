#' data_page UI Function
#'
#' @description Module encapsulating all code present on the DATA tabsetpanel
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),

    shinyWidgets::verticalTabsetPanel(menuSide = "left", contentWidth = 11,
      
      shinyWidgets::verticalTabPanel(title = "Datasets", mod_datapool_viewer_ui(id = ns("mod_datapool_viewer"))),
      shinyWidgets::verticalTabPanel(title = "Import Dataset", mod_data_import_ui(id = ns("mod_data_import")))
      #shinyWidgets::verticalTabPanel(title = "Edit Dataset", mod_edit_dataset_ui(id = ns("mod_edit_dataset"))) 
      ),
    
    )
  
}
    
#' data_page Server Functions
#'
#' @noRd 
mod_data_page_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    #browser()
    ns <- session$ns
    mod_datapool_viewer_server(id = "mod_datapool_viewer", maf_data_pool = maf_data_pool)
    mod_data_import_server(id = "mod_data_import", maf_data_pool = maf_data_pool)
    #mod_edit_dataset_server(id = "mod_edit_dataset", maf_data_pool = maf_data_pool)
  })
}
    
## To be copied in the UI
# mod_data_page_ui("data_page_ui_1")
    
## To be copied in the server
# mod_data_page_server("data_page_ui_1")
