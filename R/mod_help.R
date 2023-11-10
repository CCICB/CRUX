#' manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
      heading = 'Learning Resources',
      "To learn how to use ", tags$strong("CRUX"), " please consult the ", link(url = "https://crux-docs.readthedocs.io/en/latest/index.html", text = tags$strong("manual")), ". If you prefer ", tags$strong("video tutorials")," see the ", link(url = "https://www.youtube.com/channel/UCz3A5pNZOTjR5vrD-pR26qg", newtab = TRUE, text = tags$strong("CRUX Youtube Channel"))
      ),
    shinyWidgets::panel(heading = "Bugs and New Features" ,"Help us make CRUX better! Report bugs and request new features by sending us an ",
                        link(url = "mailto:selkamand@ccia.org.au", text = tags$strong('email')), 
                        '. Alternatively, if you have a github account report issues using the ', tags$a(target="_blank", tags$strong("CRUX Github Page"), href ="https://github.com/CCICB/CRUX/issues/new/choose")),
    shinyWidgets::panel(heading = "Contact Us","For help with any CRUX related queries, please ",link(url = "mailto:selkamand@ccia.org.au", text = tags$strong('contact us')))
  )
}
    
#' manual Server Functions
#'
#' @noRd 
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_help_ui("manual_ui_1")
    
## To be copied in the server
# mod_help_server("manual_ui_1")
