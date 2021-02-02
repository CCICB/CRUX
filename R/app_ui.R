#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    #Allow us to use tooltips and popovers
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),
    
    #Use ShinyJS
    shinyjs::useShinyjs(),
    
    #Use animations
    #shinyanimate::withAnim(),
    
    #Use sweetalerts (pretty modals)
    shinyWidgets::useSweetAlert(),
    
    # List the first level UI elements here 
    fluidPage(
      h1("shinymaftoolsr"),
      

# Set background colour ---------------------------------------------------

      
      shinyWidgets::setBackgroundColor(color = "#F8F8FF"),
      
      shiny::tabsetPanel(
        shiny::tabPanel(title = "Data", mod_data_page_ui(id = "mod_data_page")),
        shiny::tabPanel(title = "Pan Cohort Statistics", mod_pan_cohort_statistics_ui(id = "mod_pan_cohort_statistics")),
        #shiny::tabPanel(title = "Compare Cohorts", moduleCompareCohortsUI(id = "mod_compare_cohorts")),
        shiny::tabPanel(title = "Compare Cohorts", mod_compare_cohorts2_ui(id = "mod_compare_cohorts2")),
        shiny::tabPanel(title = "Enrichment", moduleEnrichmentAnalysisUI(id = "mod_enrichment_analyis")),
        shiny::tabPanel(title = "External Tools"),
        shiny::tabPanel(title = "Utilities", moduleUtilitiesUI(id = "mod_utilities")),
        shiny::tabPanel(title = "Help")
        #shiny::tabPanel(title="Test Space", mod_utilities_create_clinical_data_spreadsheet_ui(id = "mod_create_clinical_data_spreadsheet"))
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  #Allow shinybs to actually work
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinymaftoolsr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

