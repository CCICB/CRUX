moduleEnrichmentAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(title = "Clinical Enrichment", 
               
               shinyWidgets::panel(heading = "Step 1: Select Data",
                                   mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_dataset_from_maf_data_pool"), panel = FALSE)
                                   
               ),
               icon_down_arrow(),br(),
               
               shinyWidgets::panel(heading = "Step 2: Configure Analysis",
                                   #uiOutput(outputId = ns("out_pick_clinical_feature")),
                                   mod_select_maf_clinical_data_column_ui(id = ns('mod_pick_clinical_features'), label = "Clinical Feature", ),
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
  assertions::assert_reactive(maf_data_pool)
  
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
                 maf_data_wrapper = mod_select_maf_dataset_wrapper_server(id = "mod_select_dataset_from_maf_data_pool", maf_data_pool)
                 maf <- reactive({ validate(need(!is.null(maf_data_wrapper()),message = "Please select a dataset ..." )); return(maf_data_wrapper()$loaded_data) })
                 
                 
                 has_clinical_data <- reactive({ return(ncol(maf()@clinical.data) > 1) })
                 
                 clinical_feature_selected <- mod_select_maf_clinical_data_column_server(id = "mod_pick_clinical_features", maf = maf, forced_to_pick_at_least_1 = FALSE)
                 
                 minmut <- reactive({
                   sample_number <-maf()@summary %>% dplyr::filter(ID=="Samples") %>% dplyr::pull(2) %>% as.numeric()
                   validate(need(input$in_num_minmut <= sample_number, message = "MinMut must be less than the # of samples"))
                   return(input$in_num_minmut)
                 })
                 
                 
                 
                # Step 3: Check Clinical Feature ------------------------------------------
                output$out_html_is_feature_appropriateness_alerts <- renderText({
                  #Check 1: Number of levels
                  validate(need(!is.null(clinical_feature_selected()), message = "Please select a dataset ... "))
                  validate(need(!is.null(maf()), message = "Please select a dataset ... "))
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
                      text = "Selected clinical feature has >10 levels. Running the analysis may take several minutes and requires a large amount of data if meaningful conclusions are to be derived. Run anyway?", 
                      type = "warning")
                    return(html_alert("Selected clinical feature has >10 levels. Running the analysis may take several minutes and requires a large amount of data if meaningful conclusions are to be derived", status = "warning")) 
                    }
                  else {
                    analysis_ready_to_proceed(TRUE)
                    return(html_alert(paste0("Data has a good number of levels (", number_of_levels, ")"), status = "success"))
                  }
                  })
                  })
                 
                 output$out_html_feature_value_are_frequent_enough <- renderText({
                   validate(need(!is.null(clinical_feature_selected()), message = "Please select a dataset ... "))
                   validate(need(!is.null(maf()), message = "Please select a dataset ... "))
                   min_samples_warning_threshold = 4
                   
                   min_samples_of_any_level <- maftools_clinical_data_lowest_number_of_samples_per_level(maf(), clinical_feature_selected())
                   if(min_samples_of_any_level < min_samples_warning_threshold){
                    return(html_alert(paste0("Clinical feature contains at least one level composed of very few samples (< ", min_samples_warning_threshold,"). I recommend using 'Utilities => Subset') to remove these levels before running the analysis"))) 
                   }
                 })
                 
                 output$out_plot_feature_distribution <- renderPlot({
                   validate(need(!is.null(clinical_feature_selected()), message = "Please select a dataset ... "))
                   validate(need(!is.null(maf()), message = "Please select a dataset ... "))
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

