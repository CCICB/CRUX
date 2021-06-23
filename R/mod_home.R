#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    #shinyWidgets::panel(heading = "Welcome",
     #                   p("Welcome to " tags$strong(shinyMaftoos)")
    #),
    
    shinyWidgets::panel(heading = "Using CRUX", style = "height: 700px",
      img(src="img/workflows.png", style = "width: auto; height: 650px; display: block; margin: auto", align = "center")
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")
