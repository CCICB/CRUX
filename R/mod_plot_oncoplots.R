# oncoplot --------------------------------------------------------------
# Creates taglist containing the plot + options panel

mod_plot_oncoplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_oncoplot"), height = "800px") %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    shinyWidgets::panel(heading = "Options",
      fluidRow(
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_sample_names"), label = "Show Sample Names", value = FALSE) %>% col_3(style="margin-top: 24px"),
        conditionalPanel(condition = "input.in_checkbox_show_sample_names", ns=ns, numericInput(inputId = ns("in_num_fontsize_sample"), label = "Fontsize: Sample Names", min=0, step = 0.1, value = 1.0)) %>% col_2(),
        col_8()
      ),
      hr(),
      fluidRow(
        numericInput(inputId = ns("in_num_topn"), label = "Genes to plot", min=1, step = 1, value = 20)  %>% col_3(),
        mod_select_genes_ui(id = ns("mod_select_genes_to_ignore"), label = "Genes to ignore", multiple = TRUE) %>% col_3(),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_sample_plot"), label = "Show sample barplot", value = TRUE) %>% col_3(style="margin-top: 24px"),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_gene_plot"), label = "Show gene barplot", value = TRUE) %>% col_3(style="margin-top: 24px"),
      ),
      hr(),
      fluidRow(
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_titv_plot"), label = "Show TiTv barplot", value = FALSE) %>% col_3(style="margin-top: 24px"),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_draw_outline_plot"), label = "Draw outline", value = FALSE) %>% col_3(style="margin-top: 24px"),
        numericInput(inputId = ns("in_num_sepwd_genes"), label = "Size of lines separating genes", min=0.01, step = 0.1, value = 0.5)  %>% col_3(),
        numericInput(inputId = ns("in_num_sepwd_samples"), label = "Size of lines separating samples", min=0.01, step = 0.1, value = 0.25)  %>% col_3()
      ),
      hr(),
      fluidRow(
        numericInput(inputId = ns("in_num_fontsize"), label = "Fontsize", min=0, step = 0.1, value = 1.2) %>% col_3(),
        numericInput(inputId = ns("in_num_fontsize_title"), label = "Fontsize: Title", min=0, step = 0.1, value = 1.5) %>% col_3(),
        numericInput(inputId = ns("in_num_fontsize_legend"), label = "Fontsize: Legend", min=0, step = 0.1, value = 2) %>% col_3(),
        numericInput(inputId = ns("in_num_fontsize_annotation"), label = "Fontsize: annotation", min=0, step = 0.1, value = 1.5) %>% col_3(),
      ),
      
      fluidRow(
        numericInput(inputId = ns("in_num_mar_barcode"), label = "Margin: Tumor Sample Barcodes", min=0, step = 1, value = 4)  %>% col_3(),
        numericInput(inputId = ns("in_num_mar_gene"), label = "Margin: genes", min=0, step = 1, value = 8) %>% col_3(),
        numericInput(inputId = ns("in_num_legend_height"), label = "Legend height", min=0, step = 1, value = 4)  %>% col_3(),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_title"), label = "Show title", value = FALSE) %>% col_3(style="margin-top: 24px"),
      ),
      
      
      hr(),
      fluidRow(
        mod_select_maf_clinical_data_column_ui(ns("mod_select_clinical_feature"), multiple = TRUE, label = "Clinical Features")  %>% col_3(),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_sort_by_annotation"), label = "Sort by annotation", value = FALSE) %>% col_3(style="margin-top: 24px"),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_use_custom_genes"), label = "Custom genes", value = FALSE) %>% col_3(style="margin-top: 24px"),
        conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, mod_select_genes_ui(id = ns("mod_select_custom_genelist"), label = "Genes to plot",multiple = TRUE)) %>% col_3()
        #conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, uiOutput(outputId = ns("out_ui_genelist"))) %>% col_3()
      ),
      moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

mod_plot_oncoplot_server <- function(id, maf){
  moduleServer(id,
               function(input, output, session){
                 
                 clinicalData <- reactive({
                   validate(need(!is.null(maf()), "Please import maf"))
                   return(maftools::getClinicalData(maf()))
                 })
                 
                 clinical_data_selected <- mod_select_maf_clinical_data_column_server(
                   id = "mod_select_clinical_feature", 
                   maf=maf,
                   checkmark = 'oncoplottable',
                   forced_to_pick_at_least_1 = FALSE
                   )
                 
                 custom_genelist <- mod_select_genes_server("mod_select_custom_genelist", maf = maf)
                 #output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf()@data$Hugo_Symbol) %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T) })
                 
                 genes <- reactive({ 
                   if (input$in_checkbox_use_custom_genes){
                     validate(need(expr = !is.null(custom_genelist()), message = "Please select genes from the dropdown menu"))
                     return(custom_genelist()) 
                   }
                   else
                     return(NULL)
                 })
                 
                 genes_to_ignore <- mod_select_genes_server(maf = maf, id = "mod_select_genes_to_ignore")
                 
                 #clinicalFeatures()
                 
                 #Funciton call
                 plot_oncoplot <- reactive({ function() { 
                   maftools::oncoplot(maf = maf(), genes = genes(), 
                                      clinicalFeatures = clinical_data_selected(),
                                      showTumorSampleBarcodes = input$in_checkbox_show_sample_names, 
                                      fontSize = input$in_num_fontsize,
                                      SampleNamefontSize = input$in_num_fontsize_sample,
                                      titleFontSize = input$in_num_fontsize_title,
                                      legendFontSize = input$in_num_fontsize_legend,
                                      annotationFontSize = input$in_num_fontsize_annotation, 
                                      sortByAnnotation = input$in_checkbox_sort_by_annotation,
                                      barcode_mar = input$in_num_mar_barcode,
                                      gene_mar = input$in_num_mar_gene, 
                                      showTitle = input$in_checkbox_show_title,
                                      legend_height = input$in_num_legend_height, 
                                      drawColBar = input$in_checkbox_show_sample_plot,
                                      drawRowBar = input$in_checkbox_show_gene_plot, 
                                      top = input$in_num_topn, 
                                      genesToIgnore = genes_to_ignore(), 
                                      draw_titv = input$in_checkbox_show_titv_plot,
                                      drawBox = input$in_checkbox_draw_outline_plot, 
                                      sepwd_genes = input$in_num_sepwd_genes, 
                                      sepwd_samples = input$in_num_sepwd_samples
                   )
                 } 
                 })
                 output$out_plot_oncoplot <- renderPlot({plot_oncoplot()()})
                 moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_oncoplot", plotting_function = plot_oncoplot(), default_filename = "oncoplot")
               }
  )
}
