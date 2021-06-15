#' select_genes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_genes_ui <- function(id, label = "Select Genes", multiple = TRUE, actions_box = TRUE, live_search = TRUE){
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(inputId = ns("in_pick_genes"), label = label, choices = NULL, multiple = multiple, options = shinyWidgets::pickerOptions(actionsBox = actions_box, liveSearch = live_search))
  )
}
    
#' select_genes Server Functions
#'
#' @noRd 
mod_select_genes_server <- function(id, maf){
  assertthat::assert_that(is.reactive(maf))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    genelist <- reactive({
      validate(need(!is.null(maf()), message = "Please select a valid dataset"))
      maf() %>% maftools_get_all_data() %>% 
        dplyr::pull(Hugo_Symbol) %>%
        unique() %>%
        sort() %>%
        return()
      })
    
    observeEvent(maf(), {
    shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_genes", choices = genelist())
    })
    
    selected_genes <- reactive(input$in_pick_genes)
    return(selected_genes)
  })
}
    
## To be copied in the UI
# mod_select_genes_ui("select_genes_ui_1")
    
## To be copied in the server
# mod_select_genes_server("select_genes_ui_1")
