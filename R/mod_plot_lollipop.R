
# Lollipop --------------------------------------------------------------
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
#' @return
#' @export
#'
#' @examples
#' plotforest <- function(mafCompareRes, pVal = 0.05, fdr = 0.05, threshold_on_fdr, color=NULL, geneFontSize = 1.2, lineWidth = 2.2, titleSize = 1.2){
#'   message("running")
#'   if (threshold_on_fdr)
#'     maftools::forestPlot(mafCompareRes = mafCompareRes, fdr = fdr, color = color, geneFontSize = geneFontSize, lineWidth = lineWidth, titleSize = titleSize)
#'   else
#'     maftools::forestPlot(mafCompareRes = mafCompareRes, pVal = pVal,color = color, geneFontSize = geneFontSize, lineWidth = lineWidth, titleSize = titleSize)
#' }

mod_plot_lollipop_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_lollipop")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    
    shinyWidgets::panel(heading = "Transcripts",
      DT::dataTableOutput(outputId = ns("out_dt_transcript_table")) %>% shinycssloaders::withSpinner(proxy.height = "200px")
    ),
    
    shinyWidgets::panel(heading = "Options",
        uiOutput(outputId = ns("out_ui_genelist")),
        uiOutput(outputId = ns("out_ui_transcripts")),
        uiOutput(outputId = ns("out_ui_aacol")),
        
        
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_aa_labels"), label = "Label all variants?", value = FALSE),
        numericInput(inputId = ns("in_num_labposangle"), label = "Label Angle", min = 0,max = 180, step = 1, value = 0),
        numericInput(inputId = ns("in_num_labsize"), label = "Label Size", value = 3, min = 0, step = 0.2),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_cbioportal_style"), label = "cBioPortal Style annotations?", value = FALSE),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_rounded_rect"), label = "Rounded rectangles", value = FALSE, status = "primary"),
        numericInput(inputId = ns("in_num_axis_text_size"), label = "Axis text size", value = 1, min = 0, step = 0.2),
        numericInput(inputId = ns("in_num_point_size"), label = "Point size", value = 1.2, min = 0, step = 0.2),
        moduleDownloadPlotUI(id = ns("mod_download_plot")),
        
        shinyBS::bsTooltip(ns("in_pick_gene"), "Gene", placement = "right"),
        shinyBS::bsTooltip(ns("in_pick_aacol"), "Column in MAF which specifies the amino acid change (should contain <old_amino_acid> <position> <new_amino_acid>\n. e.g. S119C). Default searches for HGVSp_Short, AAChange or Protein_Change", placement = "right"),
        
        
    )
  )
}

mod_plot_lollipop_server <- function(id, maf, name_cohort = NULL){
  moduleServer(id,
    function(input, output, session){
      
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf()@data$Hugo_Symbol) %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T)) })
      output$out_ui_aacol <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_aacol"), label = "aacol", choices = c("default", colnames(maf()@data)), options = shinyWidgets::pickerOptions(liveSearch = T)) })
      output$out_ui_transcripts <- renderUI({ shinyWidgets::pickerInput(inputId = session$ns("in_pick_transcript"), label = "Transcript", choices = transcripts(), selected = longest_transcript()) })
      output$out_dt_transcript_table <- DT::renderDataTable({ transcript_table() }, options = list(scrollX = TRUE), class = "display nowrap")
      
      aacol <- reactive({ if(input$in_pick_aacol == "default") {return(NULL)} else return(input$in_pick_aacol)})
      
      label <- reactive({if(input$in_checkbox_aa_labels == T) return("all") else return(NULL)})
      gene <- reactive({ validate(need(expr = !is.null(input$in_pick_gene), message = "Please select a gene from the dropdown menu")); return(input$in_pick_gene) })
      transcript_table <- reactive({ validate(need(!is.null(gene()), message = "Choose a valid gene from the dropdown menu")); gene_name_to_transcript_table(gene(), longest_first = T) })
      transcripts <- reactive({ transcript_table()$refseq.ID })
      longest_transcript <- reactive({transcript_table() %>% dplyr::arrange(dplyr::desc(aa.length)) %>% dplyr::pull(refseq.ID) %>% head(1) })
      
      plot_lollipop <- reactive({ function(){
        maftools::lollipopPlot(maf = maf(), 
                      gene = gene(), 
                      AACol = aacol(), 
                      cBioPortal = input$in_checkbox_cbioportal_style,
                      labelPos = label(),
                      labPosAngle = input$in_num_labposangle, 
                      labPosSize = input$in_num_labsize, 
                      pointSize = input$in_num_point_size, 
                      roundedRect = input$in_checkbox_rounded_rect, 
                      refSeqID = input$in_pick_transcript) }})

      output$out_plot_lollipop <- renderPlot({plot_lollipop()()})
      
      moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_lollipop", plotting_function = plot_lollipop(), default_filename = "comparing_cohorts_lollipop")
  }
  )
}
