#' privacy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_privacy_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
      heading = "Privacy",
      "For analysing sensitive datasets, we reccomend installing CRUX binaries, or installing CRUX as an R package so you can run CRUX on a system of your choice.",
      tags$br(),tags$br(),
      "Please ensure data is anonymised and ", 
      link(url="https://www.ncbi.nlm.nih.gov/books/NBK553131/", text = tags$strong("Protected Health Information (PHI)")),
      " has been removed before bringing the data into CRUX, or any other 3rd party analysis tool.",
      tags$br(),tags$br(),
      "If using CRUX from the shinyapps.io web server, be aware that shinyapps.io is hosted on Amazon's Web Services in the us-east-1 region and the infrastructure used is not the HIPAA-compliant stack.",
      "Please see ",link(url = "https://docs.posit.co/shinyapps.io/security-and-compliance.html", text="shinyapps.io documentation")," for more information on the server infrastructure. If HIPAA compliance is required CRUX can be easily installed onto devices of your own choosing."
      )
  )
}
    
#' privacy Server Functions
#'
#' @noRd 
mod_privacy_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_privacy_ui("privacy_1")
    
## To be copied in the server
# mod_privacy_server("privacy_1")
