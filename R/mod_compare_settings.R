moduleCompareSettingsUI <- function(id, tabset_id, tab_value){
  ns <- NS(id)
  tagList(
    conditionalPanel(condition = paste0("input.", tabset_id, " == '", tab_value, "'"),
    shinyWidgets::panel(heading = "Comparing Cohorts",
    numericInput(inputId = ns("in_numeric_minmut"), label = "Minimum Number of Samples with Mutated Gene", value = 5, min = 0, step = 1),
    checkboxInput(inputId = ns("in_check_usecnv"), label = "Use CNV?", value = T),
    
    shinyBS::bsTooltip(ns("in_numeric_minmut"), "Consider only genes with minimum this number of samples mutated in at least one of the cohorts for analysis. Helful to ignore single mutated genes."),
    shinyBS::bsTooltip(ns("in_check_usecnv"), "Whether to include copy number events when comparing cohorts. Only applicable when MAF is read along with copy number data. CNV NOT IMPLEMENTED IN SHINY APP YET")
    )
    )
  )
  }


#' moduleCompareSettingsServer
#'
#' @param id 
#'
#' @return named list containing cohort comparison settings. 
#' minmut: the minimum number samples (in any cohort) which must carry a mutation in a gene for it to be included in analysis. '
#' usecnv: whether or not to include cnv data in fischer test if available.
#' @export
#'
moduleCompareSettingsServer <- function(id){
  moduleServer(id,
    function(input, output, session){
      min <- reactive({validate(need(expr = !is.null(input$in_numeric_minmut), message = "in_numeric_minmut is null")); input$in_numeric_minmut})
      cnv <- reactive({input$in_check_usecnv})
      return(list(minmut = min, usecnv = cnv))
  }
  )
}

# Copy in UI
# moduleCompareSettingsUI("some_id")

# Copy in server
# moduleCompareSettingsServer("some_id", optional_argument)
