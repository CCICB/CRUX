#' druggability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_druggability_ui <- function(id){
  ns <- NS(id)
  tagList(
    

    # Step 1: Warn for research purposes only! --------------------------------
    wellPanel(
      p(tags$strong("This resource is intended for purely research purposes. It should NOT be used for emergencies or medical or professional advice.")),
      hr(),
      p("This resource is gene level. We", tags$strong("strongly"), "reccomend also getting variant level druggability annotations from the Cancer Genome Interpreter (CGI).", 
        tags$ol(
          tags$li("Use 'external tools' module to generate a CGI compatible file"),
          tags$li("Run file in CGI and select the 'prescriptions' tab to get variant level druggability information")
          )
      )
    ),
    #Maybe switch these to wellpanels to get HTML formatting in there
    
    # Step 2: Describe Druggable Categories --------------------------------
    mod_plot_druginteractions_ui(ns("mod_plot_druginteractions"))
  )
}
    
#' druggability Server Functions
#'
#' @noRd 
mod_druggability_server <- function(id, maf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_plot_druginteractions_server("mod_plot_druginteractions", maf = maf)
  })
}
    
## To be copied in the UI
# mod_druggability_ui("druggability_ui_1")
    
## To be copied in the server
# mod_druggability_server("druggability_ui_1")
