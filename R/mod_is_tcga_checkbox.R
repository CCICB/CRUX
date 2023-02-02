#' is_tcga_checkbox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_is_tcga_checkbox_ui <- function(id, label = "Is TCGA?"){
  ns <- NS(id)
  tagList(
    shinyWidgets::awesomeCheckbox(ns("in_check_is_tcga"), label = label, value = FALSE)
  )
}
    
#' is_tcga_checkbox Server Functions
#'
#' @noRd 
mod_is_tcga_checkbox_server <- function(id, maf_dataset_wrapper){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Automaticly guess if its a TCGA dataset but allow user to override not 
    observe({ 
      validate(need(!is.null(maf_dataset_wrapper()), message = "Please select a dataset"))
      if(grepl("TCGA", toupper(maf_dataset_wrapper()$name_of_data_source)))
        shinyWidgets::updateAwesomeCheckbox(session = session, inputId = "in_check_is_tcga", value = TRUE)
      else 
        shinyWidgets::updateAwesomeCheckbox(session = session, inputId = "in_check_is_tcga", value = FALSE)
    })
    
    return(reactive(input$in_check_is_tcga))
  })
}
    
## To be copied in the UI
# mod_is_tcga_checkbox_ui("is_tcga_checkbox_ui_1")
    
## To be copied in the server
# mod_is_tcga_checkbox_server("is_tcga_checkbox_ui_1")
