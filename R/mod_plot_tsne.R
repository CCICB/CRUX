#' #' plot_tsne UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd 
#' #'
#' #' @importFrom shiny NS tagList 
#' mod_plot_tsne_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     
#'     shinyWidgets::panel(
#'       heading = "Configure tSNE",
#'       numericInput(ns("in_num_perplexity"), label = "Perplexity", value = 30, min = 1, step = 1),
#'       actionButton(inputId = ns("in_bttn_run_tsne"), label = "Run tSNE analysis",width = "100%")  
#'     ),
#'     
#'     plotOutput(ns("out_plot_tsne")) %>% shinycssloaders::withSpinner(),
#'     shinyWidgets::panel(
#'       heading = "Options",
#'       mod_select_maf_clinical_data_column_ui(ns("mod_select_clinical_data_column"))
#'     )
#'   )
#' }
#'     
#' #' plot_tsne Server Functions
#' #'
#' #' @noRd 
#' mod_plot_tsne_server <- function(id, expression_df, maf){
#'   moduleServer( id, function(input, output, session){
#'     ns <- session$ns
#'     
#'     tsne_out <- reactive({
#'       input$in_bttn_run_tsne
#'       if(input$in_bttn_run_tsne==0)
#'         validate("Click button to run tSNE analysis")
#'       
#'       isolate({
#'         validate(need(!is.null(expression_df()), message = "No expression data"))
#'         message("About to convert df to matrix")
#'         expression_matrix <- utilitybeltrna::rnaseq_df_to_matrix_gene_columns(expression_df())
#'         #browser()
#'         message("About to start running tsne")
#'         tsne_out_ <- utilitybeltrna::rnaseq_tsne(expression_matrix, perplexity = input$in_num_perplexity)
#'         message("TSNE finished")
#'       })
#'       return(tsne_out_)
#'     })
#'     
#'     selected_metadata_column <- mod_select_maf_clinical_data_column_server("mod_select_clinical_data_column", maf, forced_to_pick_at_least_1 = FALSE)
#'     
#'     clinical_data_df <- reactive({ 
#'       validate(need(!is.null(maf()), message = "Loading dataset ..."))
#'       maftools::getClinicalData(maf())
#'       })
#' 
#'     output$out_plot_tsne <- renderPlot({
#'       message("Rendering tSNE plot. Coloring by ", selected_metadata_column())
#'       utilitybeltrna::rnaseq_tsne_ggplot(rnaseq_tsne_output = tsne_out(), sample_metadata_df = clinical_data_df(), color_column = selected_metadata_column())
#'       })
#'   })
#' }
    
## To be copied in the UI
# mod_plot_tsne_ui("plot_tsne_1")
    
## To be copied in the server
# mod_plot_tsne_server("plot_tsne_1")
