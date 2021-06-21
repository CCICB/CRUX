#' survival_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_survival_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Step 1: Select dataaset-------------------------------------------------------------------
    shinyWidgets::panel(
      heading="Step 1: Select Dataset",
      mod_select_maf_dataset_wrapper_ui(ns("mod_select_dataset_wrapper"), panel = FALSE)
      ),
    
    icon_down_arrow(),br(),
    
    # Step 2: Select metadata columns containing survival info ------------------------------------------------------------------
    
    shinyWidgets::panel(
      heading = "Step 2: Select metadata columns containing survival information",
      #shinyWidgets::pickerInput(ns("in_pick_time_to_event"), label = "Time To Event", choices = NULL, multiple = FALSE, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
      #shinyWidgets::pickerInput(ns("in_pick_event_status"), label = "Event Status", choices = NULL, multiple = FALSE, options = shinyWidgets::pickerOptions(liveSearch = TRUE))
      mod_select_maf_clinical_data_column_ui(id = ns("in_pick_time_to_event"), label = "Time To Event",multiple = FALSE, tooltip_text = "Usually named days_to_last_followup"),
      mod_select_maf_clinical_data_column_ui(id = ns("in_pick_event_status"), label = "Event Status",multiple = FALSE, tooltip_text = "Usually named something like 'vital_status' or 'Overall_Survival_Status") 
    ),
    
    icon_down_arrow(),br(),
    

    # Step 3: Configure Analysis ----------------------------------------------------------------
    
    shinyWidgets::panel(
      heading = "Step 3: Configure Analysis",
      numericInput(ns("in_num_genes_to_include"), label = "Only consider the top N genes", value = 20, min = 1, step = 1),
      numericInput(ns("in_num_geneset_size"), label = "Geneset size", value = 1, min = 1, step = 1),
      numericInput(ns("in_num_minsamples"), label = "Mimimum samples", value = 5, min = 0, step = 1),
      mod_is_tcga_checkbox_ui(id = ns("mod_is_tcga_checkbox"))
    ),
    
    icon_down_arrow(), br(),
    
    # Step 4: View tabular results ----------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 4: View potentially prognostic genesets",
        DT::dataTableOutput(outputId = ns("out_dt_prognostic_genesets")) %>% shinycssloaders::withSpinner(),
      br()
    ),

    icon_down_arrow(), br(),
    
    # Step 5: Configure survival curve visualisation ----------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 5: Configure survival curve visualisation",
      shinyWidgets::pickerInput(ns("in_pick_geneset"), label = "Select Geneset", choices = NULL, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
      shinyWidgets::radioGroupButtons(ns("in_radiogroup_geneset_combination"), label = "Are mutations required in all/any genes in the geneset?", choices = c(all=FALSE, any=TRUE), selected = FALSE)
    ),
    
    icon_down_arrow(), br(),
    
    # Step 6: Visualise Survival Curves ---------------------------------------
    shinyWidgets::panel(
      heading = "Step 6: Visualise survival curve",
      #plotOutput(ns("out_plot_survival_curve")) %>% shinycssloaders::withSpinner()
      mod_plot_survival_ui(ns("mod_plot_survival"))
    ),
  )
}
    
#' survival_analysis Server Functions
#'
#' @noRd 
mod_survival_analysis_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Step 1: Select Dataset --------------------------------------------------
    maf_dataset_wrapper <- mod_select_maf_dataset_wrapper_server(id = "mod_select_dataset_wrapper", maf_data_pool = maf_data_pool, label =  "Step 1: Select Dataset")
    maf <- reactive({maf_dataset_wrapper()$loaded_data })
    clinical_data <- reactive({ maf() %>% maftools::getClinicalData()})
    

    # Step 2: Select metadata columns containing survival info ------------------------------------------------------------------
    column_time_to_event <- mod_select_maf_clinical_data_column_server(id = "in_pick_time_to_event", maf = maf, message_when_none_are_selected = "Please select column describing time to event")
    column_event_status <- mod_select_maf_clinical_data_column_server(id = "in_pick_event_status", maf = maf, message_when_none_are_selected = "Please select column describing event status")
    
    # Step 3: Configure Analysis ----------------------------------------------------------------
    is_tcga <- mod_is_tcga_checkbox_server("mod_is_tcga_checkbox", maf_dataset_wrapper)
    
    

    # Step 4: View tabular results ----------------------------------------------------
    prognostic_gene_df <- reactive({ 
      validate(need(event_column_passes_sanitychecks(column_event_status(), clinical_data()), message = "Please select valid event column"))
      validate(need(time_column_passes_sanitychecks(column_time_to_event(), clinical_data()), message = "Please select valid time column"))
      #browser()
      
      tryCatch(
        expr = { 
          maftools::survGroup(
            maf = maf(), 
            top = input$in_num_genes_to_include, 
            geneSetSize = input$in_num_geneset_size, 
            minSamples = input$in_num_minsamples, 
            time = column_time_to_event(), 
            Status = column_event_status()
          ) 
        },
        error = function(err){
          validate(paste0("Couldn't run survival analysis. This usually means either the wrong `event` or `time` columns were selected, or these columns are formatted incorrectly. Please see the manual for examples. \nFull Error Message: ",as.character(err)))
        }
      )
      
    })
    output$out_dt_prognostic_genesets <- DT::renderDataTable({ prognostic_gene_df() })
    

    # Step 5: Configure survival curve visualisation -------------------------------
    genes <- reactive({ validate(need(!is.null(maf()), message = "Loading dataset")); maf() %>% maftools::getGeneSummary() })
    most_interesting_choice <- reactive({prognostic_gene_df()[[1,1]] %>% strsplit(split = "_") %>% unlist()})
    
    observe({
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_geneset", choices = genes(), selected = most_interesting_choice())
    })
    
    selected_geneset <- reactive({
      validate(need(!is.null(input$in_pick_geneset), message = "Please select a geneset"))
      return(input$in_pick_geneset)
      })
    
    or <- reactive({input$in_radiogroup_geneset_combination})
    
    # Step 6: Visualise Survival Curves ---------------------------------------
    mod_plot_survival_server(id = "mod_plot_survival", maf = maf, geneset = selected_geneset, status = column_event_status, time = column_time_to_event, or = or, is_tcga = is_tcga)
    
    
  })
}
    
time_column_passes_sanitychecks <- function(column_name, clinical_dataframe){
  vec <- clinical_dataframe[[column_name]]
  
  if(!is.numeric(vec))
    return(FALSE)
  else
    return(TRUE)
}


event_column_passes_sanitychecks <- function(column_name, clinical_dataframe){
  vec <- clinical_dataframe[[column_name]]
  n_levels <- vec %>% unique() %>% length()
  
  
  
  if(n_levels < 2 || n_levels > 3 ){ # Ensure number of leveles is 2 or 3 (0 - censored, 1 - event, NA / "" - no observation)
    message("Survival Analysis: Incorrect number of levels: Expected 2 or 3, found: ", n_levels)
    return(FALSE)
  }
  else
    return(TRUE)
  }
## To be copied in the UI
# mod_survival_analysis_ui("survival_analysis_ui_1")
    
## To be copied in the server
# mod_survival_analysis_server("survival_analysis_ui_1")
