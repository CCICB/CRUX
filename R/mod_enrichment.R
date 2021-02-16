moduleEnrichmentAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(title = "Clinical Enrichment", 
               
               shinyWidgets::panel(heading = "Step 1: Select Data",
                                   mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui(id = ns("mod_select_dataset_from_maf_data_pool"), panel = FALSE)
                                   
               ),
               icon_down_arrow(),br(),
               
               shinyWidgets::panel(heading = "Step 2: Configure Analysis Data",
                                   uiOutput(outputId = ns("out_pick_clinical_feature")),
                                   numericInput(inputId = ns("in_num_minmut"), label = "Minimum Number of Mutations", value = 5, min = 0, step = 1)
               ),
               icon_down_arrow(),br(),
               
               shinyWidgets::panel(heading = "Step 3: Check Clinical Feature Is Appropriate",
                                   htmlOutput(outputId = ns("out_html_is_feature_appropriateness_alerts")),
                                   htmlOutput(outputId = ns("out_html_feature_value_are_frequent_enough")),
                                   plotOutput(outputId = ns("out_plot_feature_distribution"))
               ),
               icon_down_arrow(),br(),
               
               shinyWidgets::panel(heading = "Step 3: View Tabular Results",
                                   tabsetPanel(
                                     tabPanel(title = "Feature Sizes", DT::dataTableOutput(outputId = ns("out_dt_cf_size")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), br()),
                                     tabPanel(title = "Groupwise", DT::dataTableOutput(outputId = ns("out_dt_groupwise")) %>% shinycssloaders::withSpinner(proxy.height = "200px") , br()),
                                     tabPanel(title = "Pairwise", DT::dataTableOutput(outputId = ns("out_dt_pairwise")) %>% shinycssloaders::withSpinner(proxy.height = "200px"), br())
                                   )),
               icon_down_arrow(),br(),
               
               shinyWidgets::panel(heading = "Step 4: View Plots",
                                   plotOutput(outputId = ns("out_plot_enrichment_results")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
                                   shinyWidgets::panel(heading = "Options", 
                                                       numericInput(inputId = ns("in_num_pvalue"), label = "P value Threshold (plot) ", value = 0.05, min = 0, step = 0.01),
                                                       moduleDownloadPlotUI(id = ns("mod_download_enrichment_plot"))
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
                 

                # ReactiveVals ------------------------------------------------------------
                 analysis_ready_to_proceed <- reactiveVal(FALSE)
                 sweetconfirm_debounce <- reactiveVal(FALSE)
                 
                 observeEvent(input$in_confirm_run_anyway, isolate({
                              if(input$in_confirm_run_anyway == TRUE){
                                analysis_ready_to_proceed(TRUE)
                              }
                              }))
                 
                 # Select Data -------------------------------------------------------------
                 maf_unvalidated = mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server(id = "mod_select_dataset_from_maf_data_pool", maf_data_pool)
                 maf <- reactive({ validate(need(!is.null(maf_unvalidated()),message = "Loading ..." )); return(maf_unvalidated()) })
                 
                 
                 has_clinical_data <- reactive({ return(ncol(maf()@clinical.data) > 1) })
                 
                 clinical_features <- reactive({ 
                   validate(need(has_clinical_data(), message = "Please upload clinical feature file"))
                   maf() %>% maftools::getClinicalData() %>% dplyr::summarise(dplyr::across(.fns = function(x) length(unique(x)))) %>% tidyr::pivot_longer(cols = everything()) %>% dplyr::filter(value > 1) %>% dplyr::pull(name) %>% purrr::keep(.!="Tumor_Sample_Barcode") %>% return() 
                   
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
                 
                 
                # Step 3: Check Clinical Feature ------------------------------------------
                output$out_html_is_feature_appropriateness_alerts <- renderText({
                  #Check 1: Number of levels
                  validate(need(!is.null(clinical_feature_selected()), message = "Loading ... "))
                  validate(need(!is.null(maf()), message = "Loading ... "))
                  #browser()
                  clinical_feature_values <- maftools::getClinicalData(maf())[[clinical_feature_selected()]]
                  
                  isolate({
                  number_of_levels <- dplyr::n_distinct(clinical_feature_values)
                  
                  if(number_of_levels < 2 ) {
                    analysis_ready_to_proceed(FALSE)
                    return(html_alert("Too few levels for enrichment analysis", status = "danger"))
                  }
                  else if(number_of_levels == 2 ) {
                    analysis_ready_to_proceed(FALSE)
                    return(html_alert("Clinical feature has 2 levels. Better off doing a 2-cohort analysis", status = "danger")) 
                    }
                  else if(number_of_levels > 10 ) { 
                    analysis_ready_to_proceed(FALSE)
                    shinyWidgets::confirmSweetAlert(
                      session = session, 
                      inputId = "in_confirm_run_anyway", 
                      title = "Run analysis?", 
                      text = "Selected clinical feature has >10 levels. Running the analysis may take a while and requires a large amount of data if meaninful conclusions are to be derived. Run anyway?", 
                      type = "warning")
                    return(html_alert("Selected clinical feature has >10 levels. Running the analysis may take a while and requires a large amount of data if meaninful conclusions are to be derived", status = "warning")) 
                    }
                  else {
                    analysis_ready_to_proceed(TRUE)
                    return(html_alert(paste0("Data has a good number of levels (", number_of_levels, ")"), status = "success"))
                  }
                  })
                  })
                 
                 output$out_html_feature_value_are_frequent_enough <- renderText({
                   validate(need(!is.null(clinical_feature_selected()), message = "Loading ... "))
                   validate(need(!is.null(maf()), message = "Loading ... "))
                   min_samples_warning_threshold = 4
                   
                   min_samples_of_any_level <- maftools_clinical_data_lowest_number_of_samples_per_level(maf(), clinical_feature_selected())
                   if(min_samples_of_any_level < min_samples_warning_threshold){
                    return(html_alert(paste0("Clinical feature contains at least one level composed of very few samples (< ", min_samples_warning_threshold,"). I recommend using 'Utilities => Subset') to remove these levels before running the analysis"))) 
                   }
                 })
                 
                 output$out_plot_feature_distribution <- renderPlot({
                   validate(need(!is.null(clinical_feature_selected()), message = "Loading ... "))
                   validate(need(!is.null(maf()), message = "Loading ... "))
                   maftools_clinical_data_visually_summarise(maf = maf(), clinical_feature = clinical_feature_selected())
                   })
                 
                 
                 plot_clinical_enrichment <- reactive({ function() { maftools::clinicalEnrichment(maf = maf(), clinicalFeature = clinical_feature_selected(), minMut = minmut())} })
                 clinical_enrichment <- reactive({ 
                   validate(need(analysis_ready_to_proceed(), message = "Cannot proceed with enrichment analysis until issues are resolved"))
                   plot_clinical_enrichment()() 
                   } )
                 
                 output$out_dt_cf_size <- DT::renderDataTable(clinical_enrichment()$cf_sizes, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
                 output$out_dt_groupwise <- DT::renderDataTable(clinical_enrichment()$groupwise_comparision, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
                 output$out_dt_pairwise <- DT::renderDataTable(clinical_enrichment()$pairwise_comparision, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
                 output$out_plot_enrichment_results <- renderPlot({ maftools::plotEnrichmentResults(enrich_res = clinical_enrichment(), pVal = input$in_num_pvalue ) })
                 moduleDownloadPlotServer(id = "mod_download_enrichment_plot", session_parent = session, plotOutputId = "out_plot_enrichment_results", plotting_function = plot_clinical_enrichment(), default_filename = "enrichment")
               }
  )
}

