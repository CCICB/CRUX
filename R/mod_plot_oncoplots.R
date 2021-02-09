# oncoplot --------------------------------------------------------------
# Creates taglist containing the plot + options panel

mod_plot_oncoplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_oncoplot")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    shinyWidgets::panel(heading = "Options",
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_sample_names"), label = "Show Sample Names", value = FALSE),
          conditionalPanel(condition = "input.in_checkbox_show_sample_names", ns=ns, numericInput(inputId = ns("in_num_fontsize_sample"), label = "Fontsize: Sample Names", min=0, step = 0.1, value = 1.0)),
          
          numericInput(inputId = ns("in_num_fontsize"), label = "Fontsize", min=0, step = 0.1, value = 0.8),
          numericInput(inputId = ns("in_num_fontsize_title"), label = "Fontsize: Title", min=0, step = 0.1, value = 1.5),
          numericInput(inputId = ns("in_num_fontsize_legend"), label = "Fontsize: Legend", min=0, step = 0.1, value = 1.2),
          numericInput(inputId = ns("in_num_fontsize_annotation"), label = "Fontsize: annotation", min=0, step = 0.1, value = 1.5),
          
          moduleGetColumnNameUI(ns("mod_select_clinical_feature")),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_sort_by_annotation"), label = "Sort by annotation", value = FALSE),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_use_custom_genes"), label = "Custom genes", value = FALSE),
        
        conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, uiOutput(outputId = ns("out_ui_genelist"))),
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
      
      clinical_data_selected <- moduleGetColumnNameServer(id = "mod_select_clinical_feature", named_data = clinicalData, label = "Clinical Feature", multiple_selectable = T)
      
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf()@data$Hugo_Symbol) %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T) })
      
      genes <- reactive({ 
        if (input$in_checkbox_use_custom_genes){
          validate(need(expr = !is.null(input$in_pick_gene), message = "Please select genes from the dropdown menu"))
          return(input$in_pick_gene) 
        }
        else
          return(NULL)
        })
      
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
                 sortByAnnotation = input$in_checkbox_sort_by_annotation
                 )
        } 
        })
      output$out_plot_oncoplot <- renderPlot({plot_oncoplot()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_oncoplot", plotting_function = plot_oncoplot(), default_filename = "oncoplot")
  }
  )
}
