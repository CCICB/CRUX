
# cobarplot --------------------------------------------------------------
# Creates taglist containing the plot + options panel

modulePlotCobarplotUI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_cobarplot")) %>% shinycssloaders::withSpinner(), 
    shinyWidgets::panel(heading = "Options",
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_use_custom_genes"), label = "Custom genes", value = FALSE),
        conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, uiOutput(outputId = ns("out_ui_genelist"))),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_show_percent"), label = "Show Percentages", value = T),
        moduleDownloadPlotUI(id = ns("mod_download_plot"))
    )
  )
}

modulePlotCobarplotServer <- function(id, maf1, name_cohort1 = NULL, maf2, name_cohort2 = NULL){
  moduleServer(id,
    function(input, output, session){
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf1()@data$Hugo_Symbol, maf2()@data$Hugo_Symbol) %>% sort %>% unique(), options = list( `live-search` = TRUE), multiple = T) })

      genes <- reactive({
        if (input$in_checkbox_use_custom_genes){
          validate(need(expr = !is.null(input$in_pick_gene), message = "Please select genes from the dropdown menu"))
          return(input$in_pick_gene)
        }
        else
          return(NULL)
        })
      
      plot_cobarplot <- reactive({ 
        validate(need(!is.null(maf1()) & !is.null(maf2()), "Please import MAF file"))
        function() {maftools::coBarplot(m1 = maf1(), m2=maf2(), m1Name = name_cohort1(), m2Name = name_cohort2(), genes = genes(), showPct = input$in_checkbox_show_percent)} 
        })
      output$out_plot_cobarplot <- renderPlot({plot_cobarplot()()})
      
      moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_cobarplot", plotting_function = plot_cobarplot(), default_filename = "cobarplot")
  }
  )
}
