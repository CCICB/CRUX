#' data_importt_step_2 UI Function
#'
#' @description Step 2 of adding a MAF object to to the data pool: USER input -> Metadata. 
#' UI provides GUI for users to input metadata used when adding maf to the data pool.
#' 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_step2_ui <- function(id){
  ns <- NS(id)
    

  tagList(
      fluidRow(
        col_4(
            uiOutput(outputId = ns("out_ui_display_name_style")),
            textInput(inputId = ns("in_text_display_name"), label = "Display Name", placeholder = "My Datasets Name", width = "100%"),
          uiOutput(ns("bla"))
        ),
        col_4(
          textInput(inputId = ns("in_text_short_name"), label = "Short Name", placeholder = "e.g. ALL", width = "100%")
          
        ),
        col_4(
          textInput(inputId = ns("in_text_data_source"), label = "Data Source", placeholder = "e.g. PRISM", width = "100%")
        )
      ),
      fluidRow(
        col_12(
          textInput(inputId = ns("in_text_description"), label = "Description", placeholder = "Australian pediatric cancer patients enrolled in PRISM", width = "100%")
        )
      )
  )

}
    
#' data_import_step_2 Server Functions
#' 
#' Returns a list of user-filled metadata. Do \strong{NOT} evaluate any of the properties before checking all_valid == TRUE (list) (reactive).
#' 
#' @param default_data_source default data source value 
#' @param default_display_name  default display name value 
#' @param default_short_name default short name  value 
#' @param default_description default description value 
#' @param id Internal parameters for {shiny}.
#'
#' @return named list with elements: 
#' \enumerate{
#' \item all_valid (bool)
#' \item display_name (string)
#' \item short_name (string)
#' \item data_source (string)
#' \item description (string)
#' }
#' 
#' @details 
#' The list itself is reactive, so to access elements do:
#' 
#' \preformatted{
#' metadata <- mod_data_import_step2_server(id="my_id")
#' 
#' # In a reactive context:
#' observe({
#'     metadata()$all_valid
#'     metadata()$display_name
#'     metadata()$short_name
#'     metadata()$data_source
#'     metadata()$description
#' })
#' }
mod_data_import_step2_server <- function(id, default_data_source=reactive(NULL), default_display_name=reactive(NULL), default_short_name=reactive(NULL), default_description=reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Set Up Defaults ---------------------------------------------------------
    observeEvent(default_data_source(), {
      updateTextInput(session = session, inputId = "in_text_data_source", value = default_data_source())
      })
    
    observeEvent(default_display_name(), {
      updateTextInput(session = session, inputId = "in_text_display_name", value = default_display_name())
    })
    
    observeEvent(default_short_name(), {
      updateTextInput(session = session, inputId = "in_text_short_name", value = default_short_name())
    })
    
    observeEvent(default_description(), {
      updateTextInput(session = session, inputId = "in_text_description", value = default_description())
    })
    

    # Validity ------------------------------------------------
    display_name_valid = reactive({
      text_is_non_zero_string(input$in_text_display_name)
    })
    
    short_name_valid = reactive({
      text_is_non_zero_string(input$in_text_short_name)
    })
    
    data_source_valid = reactive({
      text_is_non_zero_string(input$in_text_data_source)
    })
    
    description_valid = reactive({
      text_is_non_zero_string(input$in_text_description)
    })
    
    all_inputs_valid <- reactive({
      validity_vec <- c(display_name_valid(), short_name_valid(), data_source_valid(), description_valid())
      return(all(validity_vec))
    })
    

    # Return list ----------------------------------------------------------
    return_package <- reactive({
      list(
        all_valid = all_inputs_valid(),
        display_name = input$in_text_display_name,
        short_name = input$in_text_short_name,
        data_source = input$in_text_data_source,
        description = input$in_text_description
        )
      })
    
    return(return_package)
    
})
}
    
## To be copied in the UI
# mod_data_importt_step_2_ui("data_importt_step_2_ui_1")
    
## To be copied in the server
# mod_data_importt_step_2_server("data_importt_step_2_ui_1")
