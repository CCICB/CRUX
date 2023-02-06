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
  assertions::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_validated <- reactive({ validate(need(!is.null(maf()),message = "Please select a dataset" )); return(maf()) })
    
    maf_columns.v <- reactive({
      maf_validated()@data %>% 
        colnames()
    })
    
    observeEvent(maf_columns.v(), {
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_maf_column", choices = maf_columns.v())
    })
    
    in_pick_maf_column <- reactive({ 
      validate(need(!is.null(input[["in_pick_maf_column"]]),message = "Please select a dataset" )); return(input[["in_pick_maf_column"]]) })
    
    return(in_pick_maf_column)
  })
}

## To be copied in the UI
# mod_select_maf_column_ui("select_maf_column_ui_1")

## To be copied in the server
# mod_select_maf_column_server("select_maf_column_ui_1")


#' select_maf_column UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_maf_clinical_data_column_ui <- function(id, label = "Select Property", multiple = FALSE, tooltip_text = "", tooltip_position = "right"){
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(inputId = ns("in_pick_maf_column"), label = label, choices = c(), selected = character(0), multiple = multiple, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)) %>%
      shinyBS::tipify(title = tooltip_text, placement = tooltip_position)
  )
}

#' select_maf_column Server Functions
#'
#' @noRd 
mod_select_maf_clinical_data_column_server <- function(id, maf, forced_to_pick_at_least_1=TRUE, message_when_none_are_selected = "Please Select a Clinical Feature ..."){
  assertions::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_columns.v <- reactive({
      validate(need(!is.null(maf()),message = "Please select a dataset" ));
      message("Picking Columns") 
      maf() %>% maftools::getClinicalData() %>% 
        colnames()
    })
    
    observeEvent(maf_columns.v(), {
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_maf_column", choices = maf_columns.v(), selected = character(0))
    })
    
    in_pick_maf_column <- reactive({ 
      if(forced_to_pick_at_least_1){
        validate(need(!is.null(input[["in_pick_maf_column"]]),message = message_when_none_are_selected ))
      }
      else{
        if(is.null(input[["in_pick_maf_column"]]))
          return(NULL)
      }
      
      return(input[["in_pick_maf_column"]]) 
    })
    
    return(in_pick_maf_column)
  })
}

## To be copied in the UI
# mod_select_maf_column_ui("select_maf_column_ui_1")

## To be copied in the server
# mod_select_maf_column_server("select_maf_column_ui_1")
