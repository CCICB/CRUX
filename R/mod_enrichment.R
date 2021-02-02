moduleEnrichmentAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(title = "Clinical Enrichment", 
               
               shinyWidgets::panel(heading = "Instructions",
                     tags$ol(
                       tags$li("Import a dataset using the", tags$b( "Import MAF " ), "sidebar panel"),
                       tags$li("Import a clinical feature file using the ", tags$b(" Import clinical feature file "), "sidebar panel"),
                       tags$li("Use the ", tags$b(" Configure Analysis "), "to select your clinical feature of interest (must have at least two levels)")
                     ),
                     p(
                       "If your data is split over several MAFs, see", tags$b(" Utilites => Merge Mafs"),".", 
                       "If you don't have a clinical feature file, see", tags$b(" Utilities => Add Clinical Data"),".",
                       "For other queries, see", tags$b(" Help ")
                     )
               ),
               
               shinyWidgets::panel(heading = "Configure Analysis",
                 mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui(id = ns("mod_select_dataset_from_maf_data_pool")),
                 uiOutput(outputId = ns("out_pick_clinical_feature")),
                 numericInput(inputId = ns("in_num_minmut"), label = "Minimum Number of Mutations", value = 5, min = 0, step = 1),
                 
               ),
               
               shinyWidgets::panel(heading = "Output",
                  shinyWidgets::panel(heading = "Tables",
                     tabsetPanel(
                       tabPanel(title = "Feature Sizes", DT::dataTableOutput(outputId = ns("out_dt_cf_size")) %>% shinycssloaders::withSpinner(), br()),
                       tabPanel(title = "Groupwise", DT::dataTableOutput(outputId = ns("out_dt_groupwise")) %>% shinycssloaders::withSpinner() , br()),
                       tabPanel(title = "Pairwise", DT::dataTableOutput(outputId = ns("out_dt_pairwise")) %>% shinycssloaders::withSpinner(), br())
                       )),
                  shinyWidgets::panel(heading = "Plot",
                       plotOutput(outputId = ns("out_plot_enrichment_results")) %>% shinycssloaders::withSpinner(),
                       shinyWidgets::panel(heading = "Options", 
                             numericInput(inputId = ns("in_num_pvalue"), label = "P value Threshold (plot) ", value = 0.05, min = 0, step = 0.01),
                             moduleDownloadPlotUI(id = ns("mod_download_enrichment_plot"))
                             )
                     )
                 )
               )
      )
    )
  }

moduleEnrichmentAnalysisServer <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer(id,
    function(input, output, session){

      maf = mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server(id = "mod_select_dataset_from_maf_data_pool", maf_data_pool)
      
      has_clinical_data <- reactive({ return(ncol(maf()@clinical.data) > 1) })
      
      clinical_features <- reactive({ 
        validate(need(has_clinical_data(), message = "Please upload clinical feature file"))
        maftools::getClinicalData(maf()) %>% colnames %>% tail(-1)
        })
      minmut <- reactive({
        sample_number <-maf()@summary %>% dplyr::filter(ID=="Samples") %>% dplyr::pull(2) %>% as.numeric()
        validate(need(input$in_num_minmut <= sample_number, message = "MinMut must be less than the # of samples"))
        return(input$in_num_minmut)
        })
      
      output$out_pick_clinical_feature <- renderUI({ shinyWidgets::pickerInput(inputId = session$ns("in_pick_clinical_features"), label = "Clinical Feature", choices = clinical_features(), multiple = F, options = shinyWidgets::pickerOptions(liveSearch = T)) })
      
      clinical_feature_selected <- reactive({ 
        validate(need(has_clinical_data(), message = "Please upload clinical feature file"))
        input$in_pick_clinical_features 
        })
      
      plot_clinical_enrichment <- reactive({ function() { maftools::clinicalEnrichment(maf = maf(), clinicalFeature = clinical_feature_selected(), minMut = minmut())} })
      clinical_enrichment <- reactive({ plot_clinical_enrichment()() } )
      
      output$out_dt_cf_size <- DT::renderDataTable(clinical_enrichment()$cf_sizes, options = list(scrollX = TRUE), class = "display nowrap")
      output$out_dt_groupwise <- DT::renderDataTable(clinical_enrichment()$groupwise_comparision, options = list(scrollX = TRUE), class = "display nowrap")
      output$out_dt_pairwise <- DT::renderDataTable(clinical_enrichment()$pairwise_comparision, options = list(scrollX = TRUE), class = "display nowrap")
      output$out_plot_enrichment_results <- renderPlot({ maftools::plotEnrichmentResults(enrich_res = clinical_enrichment(), pVal = input$in_num_pvalue ) })
      moduleDownloadPlotServer(id = "mod_download_enrichment_plot", session_parent = session, plotOutputId = "out_plot_enrichment_results", plotting_function = plot_clinical_enrichment(), default_filename = "enrichment")
  }
  )
}

