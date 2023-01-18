
# Rainforest --------------------------------------------------------------
# Creates taglist containing the plot + options panel


# Functions ---------------------------------------------------------------
#' plotforest: Wraps around maftools::forestPlot to more explicitly handle pval VS fdr option and allow both to be passed while onl using the value set by bool 'threshold_on_fdr'
#'
#' @param mafCompareRes 
#' @param pVal 
#' @param fdr 
#' @param color 
#' @param geneFontSize 
#' @param lineWidth 
#' @param titleSize 
#'
#'
#'
plotforest <- function(mafCompareRes, pVal = 0.05, fdr = 0.05, threshold_on_fdr, color=NULL, geneFontSize = 1.2, lineWidth = 2.2, titleSize = 1.2){
  message("running")
  if (threshold_on_fdr)
    maftools::forestPlot(mafCompareRes = mafCompareRes, fdr = fdr, color = color, geneFontSize = geneFontSize, lineWidth = lineWidth, titleSize = titleSize)
  else
    maftools::forestPlot(mafCompareRes = mafCompareRes, pVal = pVal,color = color, geneFontSize = geneFontSize, lineWidth = lineWidth, titleSize = titleSize)
}

modulePlotRainforestUI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_rainforest"), height = "650px") %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    shinyWidgets::panel(
      
      #Core UI
      fluidRow(
      
        col_1(shinyWidgets::radioGroupButtons(inputId = ns("in_radio_group_pval_vs_fdr"), label = "Significance", choices = c("P-value", "FDR"), selected = "FDR")),
      col_2(
        conditionalPanel(condition = "input.in_radio_group_pval_vs_fdr == 'P-value'", ns = ns, numericInput(inputId = ns("in_numeric_pvalue"), label = "p-value threshold", value = 0.05, min = 0, max = 1, step = 0.01)),
        conditionalPanel(condition = "input.in_radio_group_pval_vs_fdr == 'FDR'", ns = ns, numericInput(inputId = ns("in_numeric_fdr"), label = "fdr threshold", value = 0.05, min = 0, max = 1, step = 0.01))
      ),
      
      col_3(colourpicker::colourInput(inputId = ns("in_colours_cohort1"), label = textOutput(ns("out_name_cohort1")), palette = "square", value = "#B0004E")), 
      col_3(colourpicker::colourInput(inputId = ns("in_colours_cohort2"), label = textOutput(ns("out_name_cohort2")), palette = "square", value = "#2A4CE3"))
      
      ),
      
      fluidRow(
        col_4(numericInput(inputId = ns("in_numeric_gene_fontsize"), label = "Gene Fontsize", value = 1.2, min = 0.01, step = 0.6)),
        col_4(numericInput(inputId = ns("in_numeric_linewidth"), label = "Line Width", value = 2.2, min = 0.01, step = 0.2)),
        col_4(numericInput(inputId = ns("in_title_size"), label = "title size", value = 1.2, min = 0.01, step = 0.2))
      ),
      moduleDownloadPlotUI(id = ns("mod_download_plot")),
      
      #Tooltips
      shinyBS::bsTooltip(ns("in_radio_group_pval_vs_fdr"), "Should we use raw  or fdr corrected p values to determine which genes are visible in the plot"),
      shinyBS::bsTooltip(ns("in_numeric_pvalue"), "P value threshold which determines which genes are visible in the plot"),
      shinyBS::bsTooltip(ns("in_numeric_fdr"), "fdr threshold which determines which genes are visible in the plot")
      
    )
  )
}

modulePlotRainforestServer <- function(id, mafCompareObject){
  moduleServer(id,
    function(input, output, session){
      cohortNames <- reactive({ mafCompareObject()$SampleSummary$Cohort })
      name_cohort1 <- reactive(mafCompareObject()$SampleSummary$Cohort[1])
      name_cohort2 <- reactive ({ mafCompareObject()$SampleSummary$Cohort[2] })
      output$out_name_cohort1 <- renderText({ name_cohort1() })
      output$out_name_cohort2 <- renderText({ name_cohort2() })
      
      plot_rainforest <- reactive({ 
        validate(need(!is.null(mafCompareObject()), "Please import MAF file"))
        function() { 
        plotforest(mafCompareRes = mafCompareObject(), pVal = input$in_numeric_pvalue, fdr = input$in_numeric_fdr, threshold_on_fdr = (input$in_radio_group_pval_vs_fdr=="FDR"), color =c(input$in_colours_cohort1, input$in_colours_cohort2), geneFontSize = input$in_numeric_gene_fontsize, lineWidth = input$in_numeric_linewidth, titleSize = input$in_title_size)
        }
      })
      
      output$out_plot_rainforest <- renderPlot({
        plot_rainforest()()
        #plotforest(mafCompareRes = mafCompareObject(), pVal = input$in_numeric_pvalue, fdr = input$in_numeric_fdr, threshold_on_fdr = (input$in_radio_group_pval_vs_fdr=="FDR"), color =c(input$in_colours_cohort1, input$in_colours_cohort2), geneFontSize = input$in_numeric_gene_fontsize, lineWidth = input$in_numeric_linewidth, titleSize = input$in_title_size)
        })
      # output$out_download_rainforest <- shiny::downloadHandler(filename = "compare_two_cohorts_rainforest.svg",
      #                                                          content = function(file) {
      #                                                            svg(file)
      #                                                            plotforest(mafCompareRes = mafCompareObject(), pVal = input$in_numeric_pvalue, fdr = input$in_numeric_fdr, threshold_on_fdr = (input$in_radio_group_pval_vs_fdr=="FDR"), color =c(input$in_colours_cohort1, input$in_colours_cohort2), geneFontSize = input$in_numeric_gene_fontsize)
      #                                                            dev.off()
      #                                                            })
      moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_rainforest", plotting_function = plot_rainforest(), default_filename = "rainforest")
  }
  )
}
