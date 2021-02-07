#' select_maf_column UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_maf_column_ui <- function(id, label = "Select Property"){
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(inputId = ns("in_pick_maf_column"), label = label, choices = c(), options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE))
  )
}
    
#' select_maf_column Server Functions
#'
#' @noRd 
mod_select_maf_column_server <- function(id, maf){
  utilitybeltshiny::assert_reactive(maf)
 
   moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_validated <- reactive({ validate(need(!is.null(maf()),message = "Loading ..." )); return(maf()) })
    
    maf_columns.v <- reactive({
      maf_validated()@data %>% 
        colnames()
      })
    
    observeEvent(maf_columns.v(), {
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_maf_column", choices = maf_columns.v())
      })
    
    in_pick_maf_column <- reactive({ 
      validate(need(!is.null(input[["in_pick_maf_column"]]),message = "Loading ..." )); return(input[["in_pick_maf_column"]]) })
    
    return(in_pick_maf_column)
  })
}
    
## To be copied in the UI
# mod_select_maf_column_ui("select_maf_column_ui_1")
    
## To be copied in the server
# mod_select_maf_column_server("select_maf_column_ui_1")
