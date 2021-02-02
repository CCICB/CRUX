# cooncoplot --------------------------------------------------------------
# Creates taglist containing the plot + options panel

modulePlotCooncoplotUI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_cooncoplot")) %>% shinycssloaders::withSpinner(), 
    shinyWidgets::panel(heading = "Options",
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_sample_names"), label = "Show sample names", value = FALSE),
        
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_use_custom_genes"), label = "Custom genes", value = FALSE),
          conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, uiOutput(outputId = ns("out_ui_genelist"))),
        
          conditionalPanel(condition = "input.in_checkbox_show_sample_names", ns=ns, numericInput(inputId = ns("in_num_fontsize_sample"), label = "Fontsize: Sample Names", min=0, step = 0.1, value = 1.0)),
          numericInput(inputId = ns("in_num_fontsize_gene"), label = "Fontsize: Genes", min=0, step = 0.1, value = 0.8),
          numericInput(inputId = ns("in_num_fontsize_title"), label = "Fontsize: Title", min=0, step = 0.1, value = 1.5),
          numericInput(inputId = ns("in_num_fontsize_legend"), label = "Fontsize: Legend", min=0, step = 0.1, value = 1.2),
        moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

modulePlotCooncoplotServer <- function(id, maf1, name_cohort1 = NULL, maf2, name_cohort2 = NULL){
  moduleServer(id,
    function(input, output, session){
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf1()@data$Hugo_Symbol, maf2()@data$Hugo_Symbol) %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T) })
      
      genes <- reactive({ 
        if (input$in_checkbox_use_custom_genes){
          validate(need(expr = !is.null(input$in_pick_gene), message = "Please select genes from the dropdown menu"))
          return(input$in_pick_gene) 
        }
        else
          return(NULL)
        })
      
      plot_cooncoplot <- reactive({ 
        validate(need(!is.null(maf1()) & !is.null(maf2()), "Please import MAF file"))
        function() { 
          maftools::coOncoplot(m1 = maf1(), 
                     m2=maf2(), 
                     m1Name = name_cohort1(), 
                     m2Name = name_cohort2(), 
                     genes = genes(), 
                     removeNonMutated = T,
                     showSampleNames = input$in_checkbox_show_sample_names,
                     geneNamefont  = input$in_num_fontsize,
                     SampleNamefont =  input$in_num_fontsize_sample,
                     titleFontSize = input$in_num_fontsize_title,
                     legendFontSize = input$in_num_fontsize_legend
                     ) 
          } %>% return()
        })
      
      output$out_plot_cooncoplot <- renderPlot({plot_cooncoplot()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_cooncoplot", plotting_function = plot_cooncoplot(), default_filename = "coOncoplot")
  }
  )
}
