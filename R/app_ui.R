#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @export
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
    shinyWidgets::setShadow(class = "panel"),
    
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = HTML(as.character(tags$strong("CRUX"))), shinydashboard::dropdownMenu(type = "notifications", headerText = "DNA")), 
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Home", tabName = "Home", icon = icon("home"), selected = TRUE),
          shinydashboard::menuItem("Data", tabName = "Data", icon = icon("table"),
                                   shinydashboard::menuSubItem(text = "Available Data", tabName = "DataPool", icon = icon("database")),
                                   shinydashboard::menuSubItem(text = "Import Data", tabName = "DataImport", icon = icon("file-upload"))
                                   ),
          shinydashboard::menuItem("Single Cohort Statistics", tabName = "PanCohortStatistics", icon = icon("chart-bar")),
          shinydashboard::menuItem("Enrichment", tabName = "Enrichment", icon = icon("dna")),
          shinydashboard::menuItem("Survival Analysis", tabName = "Survival", icon = icon("heartbeat")),
          shinydashboard::menuItem("Copy Number Analysis", tabName = "CopyNumberAnalysis", icon = icon("stream")), #align-center
          shinydashboard::menuItem("Compare Cohorts", tabName = "CompareCohorts", icon = icon("balance-scale-left")),
          shinydashboard::menuItem("Sample Level Analysis", tabName = "SampleLevelAnalysis", icon = icon("microscope")), # maybe th icon?
          shinydashboard::menuItem("Expression", tabName = "Expression", icon = icon("th"), #badgeLabel = "Coming Soon", badgeColor = "maroon", 
                                   shinydashboard::menuSubItem(text = "Import", tabName = "ExpressionImport", icon = icon("file-upload")),
                                   shinydashboard::menuSubItem(text = "Visualise", tabName = "ExpressionAnalysis", icon = icon("th"))
                                   ), # maybe th icon?
          shinydashboard::menuItem("Utilities", tabName = "Utilities", icon = icon("toolbox"), 
                                   shinydashboard::menuSubItem(text = "Subset", tabName = "Subset", icon = icon("star-half-alt")),
                                   shinydashboard::menuSubItem(text = "Merge", tabName = "Merge", icon = icon("layer-group"))
                                   ),
          shinydashboard::menuItem("External Tools", tabName = "ExternalTools", icon = icon("sign-out-alt")),
          shinydashboard::menuItem("Manual", tabName = "Manual", icon = icon("book"), badgeLabel = "Coming Soon", badgeColor = "maroon")
        )
        ),
      
      body =shinydashboard::dashboardBody(
        # dashboardthemes::shinyDashboardThemes(
        #   theme = "onenote"
        # ),
        # 
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "Home", mod_home_ui(id = "mod_home")),
          shinydashboard::tabItem(tabName = "DataPool", mod_datapool_viewer_ui(id = "mod_datapool_viewer")),
          shinydashboard::tabItem(tabName = "DataImport", mod_data_import_ui(id = "mod_data_import")),
          shinydashboard::tabItem(tabName = "PanCohortStatistics", mod_pan_cohort_statistics_ui(id = "mod_pan_cohort_statistics")),
          shinydashboard::tabItem(tabName = "CompareCohorts", mod_compare_cohorts_ui(id = "mod_compare_cohorts")),
          shinydashboard::tabItem(tabName = "Enrichment", moduleEnrichmentAnalysisUI(id = "mod_enrichment_analyis")),
          shinydashboard::tabItem(tabName = "Survival", mod_survival_analysis_ui(id = "mod_survival_analysis")),
          shinydashboard::tabItem(tabName = "CopyNumberAnalysis", mod_cnv_ui(id = "mod_cnv_level_analysis")),
          shinydashboard::tabItem(tabName = "SampleLevelAnalysis", mod_sample_level_analysis_ui(id = "mod_sample_level_analysis")),
          shinydashboard::tabItem(tabName = "ExpressionImport", mod_expression_import_ui(id = "mod_expression_import")),
          shinydashboard::tabItem(tabName = "ExpressionAnalysis", mod_expression_analysis_ui(id = "mod_expression_analysis")),
          shinydashboard::tabItem(tabName = "ExternalTools", mod_external_tools_ui(id= "mod_external_tools")),
          #shinydashboard::tabItem(tabName = "Utilities", moduleUtilitiesUI(id = "mod_utilities")),
          shinydashboard::tabItem(tabName = "Subset", moduleSubsetMafsUI(id = "mod_subset")),
          shinydashboard::tabItem(tabName = "Merge", mod_merge_ui(id = "mod_merge")),
          shinydashboard::tabItem(tabName = "Manual")
        )
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
  
  golem::add_resource_path(
    'img', app_sys('app/img')
    )
 
  #Allow shinybs to actually work
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CRUX'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

