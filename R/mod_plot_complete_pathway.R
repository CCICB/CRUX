# complete_pathway --------------------------------------------------------------
# Creates taglist containing the plot + options panel

mod_plot_oncogenic_pathways_focused_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(outputId=ns("out_plot_complete_pathway")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    br(),
    html_alert(text = "Tumor suppressor genes are in red, and oncogenes are in blue font.",status = "info"),
    br(),
    shinyWidgets::panel(heading = "Options",
          fluidRow(              
          uiOutput(outputId = ns("out_ui_pathwaylist")) %>% col_3(),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_show_all_genes"), label = "Show all genes", value = F) %>% 
            shinyBS::tipify(title = "Include genes in pathway that are not mutated in the current dataset", placement = "right") %>% col_3(style = "margin-top: 24px"),
          
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_remove_non_mutated"), label = "Remove non-mutated samples", value = T) %>% col_3(style = "margin-top: 24px"),
          ),
          
          fluidRow(
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_show_samplenames"), label = "Show sample names", value = F) %>% col_3(style = "margin-top: 24px"),
          conditionalPanel(
            condition = "input.in_check_show_samplenames",ns = ns,
            numericInput(inputId = ns("in_num_fontsize_samplename"), label = "Sample name Font size", value = 1, min = 0, step = 0.1) %>% col_3()
            ),
          
          numericInput(inputId = ns("in_num_fontsize"), label = "Font size", value = 0.6, min = 0, step = 0.1) %>% col_3()
          ),
          moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

mod_plot_oncogenic_pathways_focused_server <- function(id, maf1){
  moduleServer(id,
    function(input, output, session){
      
      potential_pathways <- reactive({ 
        
        validate(need("data.frame" %in% class(maftools::OncogenicPathways(maf1())), "Returned object is not dataframe"))
        validate(need(nrow(maftools::OncogenicPathways(maf1())) >= 1, "No oncogenic pathways found"))
        maftools::OncogenicPathways(maf1()) %>% dplyr::pull(Pathway) %>% return()
        })
      
      output$out_ui_pathwaylist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_pathway"), label = "Pathway", choices = potential_pathways() %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T) })
      
      
      selected_pathways <- reactive({ 
          validate(need(expr = !is.null(input$in_pick_pathway), message = "Please select pathways from the dropdown menu"))
          return(input$in_pick_pathway) 
        })
      
      plot_complete_pathway <- reactive({ 
        function() { 
          maftools::PlotOncogenicPathways(
            maf= maf1(), 
            pathways = selected_pathways(), 
            fullPathway = input$in_check_show_all_genes, 
            removeNonMutated = input$in_check_remove_non_mutated, 
            showTumorSampleBarcodes = input$in_check_show_samplenames, 
            fontSize = input$in_num_fontsize, 
            SampleNamefontSize = input$in_num_fontsize_samplename
            ) 
          } 
        })
      
      output$out_plot_complete_pathway <- renderPlot({ plot_complete_pathway()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_complete_pathway", plotting_function = plot_complete_pathway(), default_filename = "full_pathway")
  }
  )
}
