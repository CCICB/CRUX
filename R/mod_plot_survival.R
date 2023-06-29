#' plot_survival UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_survival_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("out_plot"), height = "650px") %>% shinycssloaders::withSpinner(),
    
    shinyWidgets::panel(
      heading = "Options",
      moduleDownloadPlotUI(ns("mod_download_plot"))
      )
  )
}
    
#' plot_survival Server Functions
#'
#' @param id Internal parameters for {shiny}.
#' @param maf MAF object, usually produced by maftools::read_maf (reactive MAF object)
#' @param geneset character vector where each element is the hugo_symbol of a gene.  (reactive character vector)
#' @param time name of column in clinical data describing time to event (reactive string)
#' @param status name of column in clinical data describing event status (reactive string)
#' @param or should samples be classified as mutant if ANY gene in geneset is mutated (default is ALL must be mutated) (reactive flag)
#' @param is_tcga is sample from TCGA? (flag)
mod_plot_survival_server <- function(id, maf, geneset, time, status, or, is_tcga){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    stopifnot(is.reactive(maf))
    stopifnot(is.reactive(geneset))
    stopifnot(is.reactive(time))
    stopifnot(is.reactive(status))
    stopifnot(is.reactive(or))
    stopifnot(is.reactive(is_tcga))
    
    plotting_function <- reactive({
      function(){
        if(or())
          maftools::mafSurvival(maf = maf(), genes = geneset(), time = time(), Status = status(), isTCGA = is_tcga())
        else
          maftools::mafSurvGroup(maf = maf(), geneSet = geneset(), time = time(), Status = status())
      }
      })
    
    output$out_plot <- renderPlot({
      tryCatch(
        expr = { 
          plotting_function()()
        },
        error = function(err){
          if(grepl("There is only 1 group", err))
            validate(paste0("No samples have ", ifelse(or(), "any", "all"), " of the genes: ", paste0(geneset(), collapse = ","), " mutated"))
          else
            validate(as.character(err) %>% paste(collapse = " "))
        }
      )
      })
    
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot", plotting_function = plotting_function(), default_filename = "survival_curve")
  })
}
    
## To be copied in the UI
# mod_plot_survival_ui("plot_survival_ui_1")
    
## To be copied in the server
# mod_plot_survival_server("plot_survival_ui_1")
