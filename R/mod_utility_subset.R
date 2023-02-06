
# UI ----------------------------------------------------------------------


mod_utility_subset_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(
      id=ns("SubsetDatasetUI"),
      
      shinyWidgets::panel(heading = "Step 1: Select Dataset",
        #Select Dataset -------------------------------------------------------------------------
        #mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset"), panel = FALSE)
        mod_select_maf_dataset_wrapper_ui(id = ns("in_picker_dataset"), panel = FALSE)
        #textOutput(outputId = ns("debugtest"))
      ),
      
      icon_down_arrow(),br(),
      
      shinyWidgets::panel(
        heading = "Step 2: Configure Subsetting",
        wellPanel(htmlOutput(ns("out_text_maf_ready"))),
        #Subset by Sample-------------------------------------------------------------------------
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_subset_by_sample"), label = "Subset by sample"),
        conditionalPanel(condition = "input.in_checkbox_subset_by_sample", ns = ns, uiOutput(outputId = ns("out_ui_tsb"))),
        
        #Subset by Gene -------------------------------------------------------------------------
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_subset_by_gene"), label = "Subset by gene"),
        conditionalPanel(condition = "input.in_checkbox_subset_by_gene", ns = ns, wellPanel("Only keep samples which have mutations in at least one of the selected genes"), uiOutput(outputId = ns("out_ui_genelist"))),
        
        #Subset by Clinical Feature-------------------------------------------------------------------------
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_subset_by_clinical_feature"), label = "Subset by clinical feature"),
        conditionalPanel(condition = "input.in_checkbox_subset_by_clinical_feature", ns = ns, uiOutput(outputId = ns("out_ui_clinquery_field_list"))),
        conditionalPanel(condition = "input.in_checkbox_subset_by_clinical_feature && !output.field_is_numeric", ns = ns, uiOutput(outputId = ns("out_ui_clinquery_field_values"))),
        conditionalPanel(condition = "input.in_checkbox_subset_by_clinical_feature", ns = ns, plotOutput(ns("out_plot_clinfeature_summary"))),
        
        conditionalPanel(
          condition = "input.in_checkbox_subset_by_clinical_feature && output.field_is_numeric", 
          ns = ns, 
          numericInput(inputId = ns("in_num_clinquery_theshold"), label ="Value", value = 0, step = 1),
          shinyWidgets::pickerInput(inputId = ns("in_pick_clinquery_operator"), label = "Operator", choices = c("==", ">", ">=", "<","<="), selected = "==")
        ),
        
        # Subset By MAF column (a.k.a. other) ----------------------------------------------------
        shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_subset_by_other"), label = "Subset by anything else"),
        shinyBS::bsTooltip(id = ns("in_checkbox_subset_by_other"), title="Filter by any MAF column. If MAF was made using vcf2maf, this usually includes VCF INFO/FILTER fields. A common operation using these columns is to subset where FILTER == PASS", placement = "right"),
        conditionalPanel(condition = "input.in_checkbox_subset_by_other", ns = ns, uiOutput(outputId = ns("out_ui_other_field_list"))),
        conditionalPanel(condition = "input.in_checkbox_subset_by_other", ns = ns, uiOutput(outputId = ns("out_ui_maf_field_values")))
        ),
      
      icon_down_arrow(),br(),
      
      shinyWidgets::panel(heading = "Step 2.5: Debug Mode",
        # Debug Mode ----------------------------------------------------
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_debugmode"), label = "Debug Mode", value = FALSE),
        conditionalPanel(condition = "input.in_check_debugmode", ns = ns,
          shinyWidgets::panel(
            heading = "Queries", 
            textOutput(outputId = ns("out_text_clinquery")),
            textOutput(outputId = ns("out_text_otherquery")),
            textOutput(outputId = ns("out_text_summary_of_query"))
          )
        )
      ),
      
      icon_down_arrow(),br(),
      
      # MAF Summary Tables -------------------------------------------------
      shinyWidgets::panel(heading = "Step 3: Review Subset MAF", 
        mod_single_cohort_summary_tables_ui(id = ns("out_dt_subset_stats")), br()
      ),

      icon_down_arrow(),br(),
      
      # Add To Data Pool --------------------------------------------------------
      shinyWidgets::panel(heading = "Step 4: Add to Data Pool",
          #Metadata
          mod_data_import_step2_ui(id = ns("mod_data_import")),
          shinyjs::disabled(actionButton(inputId = ns("in_bttn_dataset_to_data_pool"), label = "Add to Data Pool"))
      )
    )
  )
}


# SERVER ------------------------------------------------------------------
mod_utility_subset_server <- function(id, maf_data_pool){
  moduleServer(id,
    function(input, output, session){

      # Select Dataset ----------------------------------------------------------
      maf_dataset_wrapper = mod_select_maf_dataset_wrapper_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool)

      #output$debugtest <- renderText({maf_dataset_wrapper()$short_name})

      maf <- reactive({
        validate(need(!is.null(maf_dataset_wrapper()), message = "Please select a dataset"))
        return(maf_dataset_wrapper()$loaded_data)
      })
      

      # Render Subset Instructions ----------------------------------------------
      output$out_text_maf_ready <- renderText(paste0("Choose properties to subset ", tags$strong(gsub(pattern = "_", replacement = " ", x = maf_dataset_wrapper()$display_name))))
      
      # Subset By Sample ----------------------------------------------------------
      tumour_sample_list <- reactive({ maftools::getSampleSummary(maf()) %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique() %>% as.character() %>% sort() %>% return()})
      output$out_ui_tsb <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_tsb"), label = "Samples", choices = tumour_sample_list() %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T)  })
      tsb <- reactive({ if(input$in_checkbox_subset_by_sample) return(input$in_pick_tsb) else return(NULL) })


      # Subset By Gene ----------------------------------------------------------
      gene_list <- reactive({ maf()@data$Hugo_Symbol %>% sort %>% unique %>% return() })
      output$out_ui_genelist <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_genes"), label = "Gene", choices = gene_list() %>% sort %>% unique(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T)  })
      genes <- reactive({ if(input$in_checkbox_subset_by_gene) return(input$in_pick_genes) else return(NULL) })
      samples_with_mutations_in_selected_genes <- reactive({ maftools_samples_with_mutated_gene(maf = maf(), genes()) })

      # Subset by Clinical Feature ----------------------------------------------
      clinical_feature_names_list <-reactive({  colnames(maftools::getClinicalData(maf())) %>% sort %>% unique %>% return() })
      output$out_ui_clinquery_field_list <- renderUI({  validate(need(!is.null(clinical_feature_names_list()), message = "Loading Clinical Feature Names")); shinyWidgets::pickerInput(inputId = session$ns("in_pick_clinquery_field"), label = "Field", choices = clinical_feature_names_list(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = F)  })
      clinquery_field_values <- reactive({  validate(need(!is.null(input$in_pick_clinquery_field), message = "Please select a clinical feature")); maftools::getClinicalData(maf())[[input$in_pick_clinquery_field]] %>% sort %>% unique})
      output$out_ui_clinquery_field_values <- renderUI({  validate(need(!is.null(clinquery_field_values()), message = "Populating clinquery_field_values"));   shinyWidgets::pickerInput( inputId = session$ns("in_pick_clinquery_values"), label = "Value", choices = clinquery_field_values(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T)  })

      field_is_numeric <- reactive({ validate(need(!is.null(maf()) && !is.null(input$in_pick_clinquery_field), message = "Waiting on Dataset and clinquery field"));  maftools::getClinicalData(maf())[[input$in_pick_clinquery_field]] %>% is.numeric() })
      output$field_is_numeric <- reactive({ validate(need(!is.null(maf()), message = "Waiting on Dataset"));field_is_numeric()})
      outputOptions(output, name = "field_is_numeric", suspendWhenHidden=FALSE)

      clinquery <- reactive({
        #browser()
        if(input$in_checkbox_subset_by_clinical_feature == FALSE)
          return(NULL)
        else if (field_is_numeric()){
          validate(need(!is.null(input$in_pick_clinquery_field) && !is.null(input$in_pick_clinquery_operator) && !is.null(input$in_num_clinquery_theshold), message = "Please Select a Valid Field, Value/Threshold, and Operator"))
         return(
           paste(paste0("`",input$in_pick_clinquery_field, "`"), input$in_pick_clinquery_operator, input$in_num_clinquery_theshold, sep = " ")
           )
        }
        else{
          validate(need(!is.null(input$in_pick_clinquery_values), message = "Please Select a Value of Clinical Feature to Subset on"))
           individual_queries.vec <- vapply(X = input$in_pick_clinquery_values, FUN = function(value){
             paste0(paste0("`",input$in_pick_clinquery_field, "`"), " == '", maftools_escape_special_characters(value), "'") %>%
               return()
           }, FUN.VALUE = "character")
           concatenated_queries <- paste(individual_queries.vec, collapse=" | ")
           #message("CLINQUERY: ", concatenated_queries)
           return(concatenated_queries)
        }
      })

      #Plot clinical feature information
      output$out_plot_clinfeature_summary <- renderPlot({
        #Add detection of what happens at gene level
        maftools_clinical_data_visually_summarise(maf = maf(), clinical_feature = input$in_pick_clinquery_field, threshold = input$in_num_clinquery_theshold, selected_items = input$in_pick_clinquery_values)
        })
      
      # Subset by MAF column ----------------------------------------------------
      maf_fields <- reactive({ colnames(maf()@data) %>% sort %>% unique %>% return() })
      output$out_ui_other_field_list <- renderUI({ validate(need(!is.null(maf_fields()), message = "Loading MAF fields")); shinyWidgets::pickerInput( inputId = session$ns("in_pick_field"), label = "Field", choices = maf_fields(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = F)  })
      maf_field_levels <- reactive({ validate(need(!is.null(input$in_pick_field), message = "Loading MAF fields")); maf()@data[[input$in_pick_field]] %>% sort %>% unique})
      output$out_ui_maf_field_values <- renderUI({ validate(need(!is.null(maf_field_levels()), message = "Loading Possible Options")); shinyWidgets::pickerInput( inputId = session$ns("in_pick_field_values"), label = "Value", choices = maf_field_levels(), options = shinyWidgets::pickerOptions(liveSearch = T, actionsBox = T), multiple = T)  })

      query <- reactive({
        if(input$in_checkbox_subset_by_other && !is.null(input$in_pick_field_values)) {
          individual_queries.vec <- vapply(X = input$in_pick_field_values, FUN = function(value){
              paste0("`",input$in_pick_field, "`", " == '", value, "'") %>%
              return()
            }, FUN.VALUE = "character")

          concatenated_queries <- paste(individual_queries.vec, collapse=" | ")

          message(concatenated_queries)

          return(concatenated_queries)
        }
        else
            return(NULL)
        })

      

      # Summarise Subset Conditions in plain text---------------------------------------------------------
      subset_text_summary_genes <- reactive({
        if(is.null(genes()))
          return(NULL)
        else{
         if(length(genes()) > 3)
           return(paste0("Subset by Genes: ", length(genes())))
          else
            return(paste0("Subset by Genes: ", paste0(genes(), collapse = ",")))
        }
      })
      
      subset_text_summary_tsb <- reactive({
        if(is.null(tsb()))
          return(NULL)
        else{
          if(length(tsb()) > 3)
            return(paste0("Subset by Samples: ", length(tsb())))
          else
            return(paste0("Subset by Samples: ", paste0(tsb(), collapse = ",")))
        }
      })
      
      subset_text_summary_clinical_query <- reactive({
        if(is.null(clinquery()))
          return(NULL)
        else
          paste0("Clinical Query: ", clinquery()) 
      })
      
      subset_text_summary_query <- reactive({
        if(is.null(query()))
          return(NULL)
        else
          paste0("Other Query: ", query()) 
      })

      subset_text_summary_all <- reactive({
        paste(c(subset_text_summary_tsb(), subset_text_summary_genes(), subset_text_summary_clinical_query(), subset_text_summary_query()), collapse = " | ") 
        })
      
      # Debug mode --------------------------------------------------------------
      output$out_text_summary_of_query <- renderText({ subset_text_summary_all() })
      
      # Subset MAF --------------------------------------------------------------
      subset_maf <- reactive({
        #browser()
        validate(need(!is.null(maf()), "Please import MAF"))
        shiny::validate(need(!is.null(genes()) | !is.null(tsb()) | !is.null(query()) | !is.null(clinquery()), message = "Please provide sample names, genes, or some other maf column to subset by"))
        subset_maf_ <- 
          tryCatch(
            expr = { 
              maftools::subsetMaf(maf = maf(), tsb = unique(c(samples_with_mutations_in_selected_genes(), tsb())), clinQuery = clinquery(), query = query()) %>%
                
                maftools_fix_clinical_data_types()
              },
            error = function(err){
              err <- paste0(as.character(err), collapse = "\n")
              if(grepl(x = err, pattern = "Subsetting has resulted in zero non-synonymous variants"))
                validate("Subsetting has resulted in zero non-synonymous variants")
              else if (grepl(x=err, pattern = "None of the samples meet the clinical query"))
               validate("None of the samples meet the clinical query")
              else
                validate(as.character(err))
            }
          )
      })


      


      # MAF Summary Tables ------------------------------------------------------
      mod_single_cohort_summary_tables_server(id = "out_dt_subset_stats", maf = subset_maf)


      # Add Metadata ------------------------------------------------------------
      display_name <- reactive({maf();subset_maf(); maf_dataset_wrapper()$display_name}) #added the first statments to trigger function when relevant items change
      short_name <- reactive({maf(); subset_maf(); maf_dataset_wrapper()$short_name})
      data_source <- reactive({maf();subset_maf(); maf_dataset_wrapper()$name_of_data_source})
      
      default_display_name <- reactive({ 
        if(is.null(genes()) && is.null(tsb()) && is.null(query()) && is.null(clinquery())) return(NULL)
        paste0(display_name()," [",subset_text_summary_all(),"]") 
        })
      default_short_name <- reactive({ paste0(short_name(), "_", "Subset") })
      default_description <- reactive({ 
        if(is.null(genes()) && is.null(tsb()) && is.null(query()) && is.null(clinquery())) return(NULL)
        paste0("Dataset '", display_name(), "' subset using the query: ", subset_text_summary_all()) 
        })
      
      metadata=mod_data_import_step2_server(
        id = "mod_data_import", 
        default_data_source=data_source, 
        default_display_name = default_display_name, 
        default_short_name = default_short_name, 
        default_description = default_description
        )

      # Add to Data Pool --------------------------------------------------------
      #Update Button
      observeEvent(metadata()$all_valid,{
        validate(need(!is.null(subset_maf()), message = "Please Subset Data"))
        isolate({
          if(metadata()$all_valid)
            shinyjs::enable(id = "in_bttn_dataset_to_data_pool")
          else 
            shinyjs::disable(id = "in_bttn_dataset_to_data_pool")
          })
        })


      #Add dataset to data pool
      observeEvent( input$in_bttn_dataset_to_data_pool, {

        isolate({
          validate(need(metadata()$all_valid == TRUE, message = "Please import valid metadata"))
            new_maf_dataset_wrapper = new_maf_dataset_wrapper(
              maf_data_pool = maf_data_pool(),
              display_name = metadata()$display_name,
              short_name = metadata()$short_name,
              clinical_data = maftools::getClinicalData(subset_maf()),
              name_of_data_source = metadata()$data_source,
              data_description = metadata()$description,
              unique_name = maf_data_pool_make_name_unique(maf_data_pool = maf_data_pool(), name = metadata()$display_name),
              is_dataset_downloadable = FALSE,
              is_dataset_loadable = FALSE,
              loaded_data = subset_maf(),
              start_status = "ready",
              function_to_download_data = function(x){NULL},
              function_to_load_data = function(x) {NULL},
              local_path_to_data = "NONE", datatype_of_stored_object = "NA",
              derived_from = NA, # Should be maf_dataset_wrapper
              number_of_samples = maftools_number_of_samples(subset_maf())
            ) %>%
              maf_data_wrapper_add_rnaseq(rnaseq_path = maf_dataset_wrapper()$rnaseq_filepath) #Copy over RNA from previous maf_data_wrapper

            new_maf_data_pool = maf_data_pool_add_dataset(maf_data_pool = maf_data_pool(), maf_dataset_wrapper = new_maf_dataset_wrapper)

            #Update maf_data_pool
            maf_data_pool(new_maf_data_pool)

            shinyWidgets::sendSweetAlert(session = session, title = "Success !!", text = "Dataset has been successfully imported! ", type = "success")
            shinyjs::reset(id = "SubsetDatasetUI")
            shinyjs::disable("in_bttn_dataset_to_data_pool")
          #Reset Inputs
        })
      })
      
  }
  )
}

# Copy in UI
# moduleSubsetUI("some_id")

# Copy in server
# moduleSubsetServer("some_id", optional_argument)
