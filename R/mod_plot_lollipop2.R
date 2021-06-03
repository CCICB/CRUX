
# Lollipop --------------------------------------------------------------
# Creates taglist containing the plot + options panel


# Functions ---------------------------------------------------------------

modulePlotLollipop2UI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_lollipop")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
    DT::dataTableOutput(outputId = ns("out_dt_transcript_table")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    
    shinyWidgets::panel(heading = "Options",
        uiOutput(outputId = ns("out_ui_genelist")),
        uiOutput(outputId = ns("out_ui_transcripts")),
        uiOutput(outputId = ns("out_ui_aacol1")),
        uiOutput(outputId = ns("out_ui_aacol2")),
        
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_m1_labels"), label = "Cohort 1: Label all variants", value = FALSE),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_m2_labels"), label = "Cohort 2: Label all variants", value = FALSE),
        numericInput(inputId = ns("in_num_labposangle"), label = "Label Angle", min = 0,max = 180, step = 1, value = 0),
        numericInput(inputId = ns("in_num_labsize"), label = "Label Size", value = 3, min = 0, step = 0.2),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_rounded_rect"), label = "Rounded rectangles", value = FALSE, status = "primary"),
        numericInput(inputId = ns("in_num_axis_text_size"), label = "Axis text size", value = 1, min = 0, step = 0.2),
        numericInput(inputId = ns("in_num_point_size"), label = "Point size", value = 1.2, min = 0, step = 0.2),
        moduleDownloadPlotUI(id = ns("mod_download_plot")),
        
        
        
        shinyBS::bsTooltip(ns("in_pick_gene"), "Gene", placement = "right"),
        shinyBS::bsTooltip(ns("in_pick_aacol1"), "Column in cohort 1 which specifies the amino acid change (should contain <old_amino_acid> <position> <new_amino_acid>\n. e.g. S119C). Default searches for HGVSp_Short, AAChange or Protein_Change", placement = "right"),
        shinyBS::bsTooltip(ns("in_pick_aacol2"), "Column in cohort 2 which specifies the amino acid change (should contain <old_amino_acid> <position> <new_amino_acid>\n. e.g. S119C). Default searches for HGVSp_Short, AAChange or Protein_Change", placement = "right")
        
    )
  )
}

modulePlotLollipop2Server <- function(id, maf1, name_cohort1 = NULL, maf2, name_cohort2 = NULL){
  moduleServer(id,
    function(input, output, session){
      
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_gene"), label = "Gene", choices = c(maf1()@data$Hugo_Symbol, maf2()@data$Hugo_Symbol) %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T)) })
      output$out_ui_aacol1 <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_aacol1"), label = "aacol1", choices = c("default", colnames(maf1()@data)), options = shinyWidgets::pickerOptions(liveSearch = T)) })
      output$out_ui_aacol2 <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_aacol2"), label = "aacol1", choices = c("default", colnames(maf2()@data)), options = shinyWidgets::pickerOptions(liveSearch = T)) })
      output$out_ui_transcripts <- renderUI({ shinyWidgets::pickerInput(inputId = session$ns("in_pick_transcript"), label = "Transcript", choices = transcripts(), selected = longest_transcript()) })
      
      output$out_dt_transcript_table <- DT::renderDataTable({ transcript_table() }, options = list(scrollX = TRUE), class = "display nowrap")
      
      aacol1 <- reactive({ if(input$in_pick_aacol1 == "default") {return(NULL)} else return(input$in_pick_aacol1)})
      aacol2 <- reactive({ if(input$in_pick_aacol2 == "default") {return(NULL)} else return(input$in_pick_aacol2)})
      
      m1_label <- reactive({if(input$in_checkbox_m1_labels == T) return("all") else return(NULL)})
      m2_label <- reactive({if(input$in_checkbox_m2_labels == T) return("all") else return(NULL)})
      gene <- reactive({ validate(need(expr = !is.null(input$in_pick_gene), message = "Please select a gene from the dropdown menu")); return(input$in_pick_gene) })
      transcript_table <- reactive({ validate(need(!is.null(gene()), message = "Choose a valid gene from the dropdown menu")); gene_name_to_transcript_table(gene(), longest_first = T) })
      transcripts <- reactive({ transcript_table()$refseq.ID })
      longest_transcript <- reactive({transcript_table() %>% dplyr::arrange(dplyr::desc(aa.length)) %>% dplyr::pull(refseq.ID) %>% head(1) })
      
      plot_lollipop <- reactive({ function(){
        maftools::lollipopPlot2(m1 = maf1(), 
                      m2 = maf2(), 
                      gene = gene(), 
                      AACol1 = aacol1(), 
                      AACol2 = aacol2(), 
                      m1_name = name_cohort1(), 
                      m2_name = name_cohort2(), 
                      m1_label = m1_label(),
                      m2_label = m2_label(),
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
