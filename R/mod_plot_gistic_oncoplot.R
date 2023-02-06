#' plot_gistic_oncoplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_gistic_oncoplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("out_plot_gistic_oncoplot")) %>% shinycssloaders::withSpinner(proxy.height = "60px"),
    
    shinyWidgets::panel(
      heading = "Options",
      
      #numericInput(ns("in_num_top_n"), label = "Number of Cytobands to Show", value = 5, min = , max = , step = )
      fluidRow(
        mod_select_maf_clinical_data_column_ui(id = ns("mod_select_clinical_feature"), multiple = TRUE, label = "Select Clinical Feature") %>% col_3(),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_show_tsb"), label = "Toggle Tumor Sample Barcodes", value = FALSE) %>% col_3(style = "margin-top: 30px"),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_sort_by_annotation"), label = "Sort by annotation", value = FALSE) %>% col_3(style = "margin-top: 30px"),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_remove_nonaltered_samples"), label = "Remove NonAltered Samples", value = TRUE) %>% col_3(style = "margin-top: 30px")
      ),
      fluidRow(
        numericInput(ns("in_num_fontsize_samplename"), label = "Fontsize: Samples", value = 0.6, min = 0.01, step = 0.1) %>% col_3(),
        numericInput(ns("in_num_fontsize_cytobands"), label = "Fontsize: Cytobands", value = 0.8, min = 0.01, step = 0.1) %>% col_3(),
        numericInput(ns("in_num_fontsize_legnend"), label = "Fontsize: Legend", value = 1.2, min = 0.01, step = 0.1) %>% col_3(),
        numericInput(ns("in_num_fontsize_annotations"), label = "Fontsize: Annotations", value = 1.2, min = 0.01, step = 0.1) %>% col_3()
      ),
      fluidRow(
        numericInput(ns("in_num_mar_gene"), label = "Margin: Genes", value = 6, min = 0.01, step = 1) %>% col_3(),
        numericInput(ns("in_num_mar_barcodes"), label = "Margin: Barcodes", value = 5, min = 0.01, step = 1) %>% col_3(),
        numericInput(ns("in_num_sepwd_genes"), label = "Sepwd: Genes", value = 0.5, min = 0.01, step = 1) %>% col_3(),
        numericInput(ns("in_num_sepwd_samples"), label = "Sepwd: Samples", value = 0.25, min = 0.01, step = 1) %>% col_3()
      ),
      
      numericInput(ns("in_num_topn"), label = "Top N", value = 10, min = 1, step = 1),
      moduleDownloadPlotUI(id = ns("mod_download_plot"))
      #mod_select_maf_column_value_ui(id = ns("mod_select_clinical_feature_value")),
    )
  )
}

#' plot_gistic_oncoplot Server Functions
#'
#' @noRd 
mod_plot_gistic_oncoplot_server <- function(id, gistic, maf){
  assertions::assert_reactive(maf)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    selected_clinical_feature <- mod_select_maf_clinical_data_column_server(id = "mod_select_clinical_feature", maf=maf, forced_to_pick_at_least_1 = FALSE)
    
    plot_gistic_oncoplot <- reactive({
      function(){
        maftools::gisticOncoPlot(
          gistic = gistic(), 
          clinicalData = maftools::getClinicalData(x = maf()), 
          clinicalFeatures = selected_clinical_feature(),
          showTumorSampleBarcodes = input$in_check_show_tsb,
          sortByAnnotation = input$in_check_sort_by_annotation, 
          removeNonAltered = input$in_check_remove_nonaltered_samples, 
          SampleNamefontSize = input$in_num_fontsize_samplename,
          fontSize = input$in_num_fontsize_cytobands,
          legendFontSize = input$in_num_fontsize_legnend,
          annotationFontSize = input$in_num_fontsize_annotations,
          gene_mar = input$in_num_mar_gene, 
          barcode_mar = input$in_num_mar_barcodes, 
          sepwd_genes = input$in_num_sepwd_genes, 
          sepwd_samples = input$in_num_sepwd_samples,
          top = input$in_num_topn
        )
      }
    })
    
    output$out_plot_gistic_oncoplot <- renderPlot({
      plot_gistic_oncoplot()();
      #Add download
      #, clinicalData = maftools::getClinicalData(x = maf()), clinicalFeatures = 'FAB_classification', sortByAnnotation = TRUE, top = 10)  
    })
      moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_gistic_oncoplot", plotting_function = plot_gistic_oncoplot(), default_filename = "GISTIC_oncoplot")
  })
}

## To be copied in the UI
# mod_plot_gistic_oncoplot_ui("plot_gistic_oncoplot_ui_1")

## To be copied in the server
# mod_plot_gistic_oncoplot_server("plot_gistic_oncoplot_ui_1")
