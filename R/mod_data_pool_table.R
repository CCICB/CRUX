#' datapool_viewer UI Function
#'
#' @description A shiny Module.
#' 
#' Returns taglist with the data pool rendered as a dataTableOutput 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_datapool_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
    shiny::wellPanel(p(strong(a("PCAWG", href="https://dcc.icgc.org/pcawg", target="_blank")), "and" , strong(a("TCGA", href="https://www.cancer.gov/about-nci/organization/ccg/research/structural-genomics/tcga", target="_blank"))," datasets are immediately available to you. To import your own data, use the ",strong("Import Dataset"), " tab."))
    ),
    shinyWidgets::panel(heading = "Available Datasets",
      DT::dataTableOutput(outputId = ns("out_dt_data_pool")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    )
  )
}
    
#' datapool_viewer Server Functions
#'
#' @noRd 
mod_datapool_viewer_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    maf_data_pool_df <- reactive({ 
      #browser()
      maf_data_pool_to_simple_dataframe(maf_data_pool())
      })
    
    maf_data_pool_datatable <- reactive({
      DT::datatable(data = maf_data_pool_df(), options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
      })
    
    output$out_dt_data_pool <- DT::renderDataTable({maf_data_pool_datatable()})
    
    # output$out_dt_data_pool_network <- sigmajs::renderSigmajs({ maf_data_pool_network() })
  })
}



## To be copied in the UI
# mod_datapool_viewer_ui("datapool_viewer_ui_1")
    
## To be copied in the server
# mod_datapool_viewer_server("datapool_viewer_ui_1")
