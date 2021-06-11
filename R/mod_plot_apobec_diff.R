#' plot_apobec_diff UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_apobec_diff_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("out_plot"), height = "650px") %>% shinycssloaders::withSpinner(),
    
    shinyWidgets::panel(
      heading = "Options",
      numericInput(ns("in_num_pval"), label = "p-value threshold for fisher's test", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput(ns("in_num_title_size"), label = "Size: title",value = 1, min = 0.01, step = 0.2),
      numericInput(ns("in_num_font_size"), label = "Size: font",value = 1.2, min = 0.01, step = 0.2),
      numericInput(ns("in_num_axis_lwd"), label = "Axis width", value = 1, min = 0.01, step = 0.5),
      
      moduleDownloadPlotUI(ns("mod_download_plot"))
    )
  )
}
    
#' plot_apobec_diff Server Functions
#' @inheritParams maftools::plotApobecDiff
mod_plot_apobec_diff_server <- function(id, maf, tnm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    plotting_function <- reactive({
      function(){
        maftools::plotApobecDiff(
          tnm = tnm(), 
          maf = maf(), 
          pVal = input$in_num_pval, 
          title_size = input$in_num_title_size, 
          axis_lwd = input$in_num_axis_lwd, 
          font_size = input$in_num_font_size
        )
      }
    })
    
    output$out_plot <- renderPlot({
      tryCatch(
        expr = { 
          plotting_function()()
        },
        error = function(err){
            validate(as.character(err) %>% paste(collapse = " "))
        }
      )
    })
    
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot", plotting_function = plotting_function(), default_filename = "apobec_difference")
  })
}
    
## To be copied in the UI
# mod_plot_apobec_diff_ui("plot_apobec_diff_ui_1")
    
## To be copied in the server
# mod_plot_apobec_diff_server("plot_apobec_diff_ui_1")
