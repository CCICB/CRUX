#' manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel("A manual for ", tags$strong("CRUX"), "is available", tags$a(target="_blank", tags$strong("here"), href ="https://crux-docs.readthedocs.io/en/latest/index.html"))
  )
}
    
#' manual Server Functions
#'
#' @noRd 
mod_manual_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_manual_ui("manual_ui_1")
    
## To be copied in the server
# mod_manual_server("manual_ui_1")
