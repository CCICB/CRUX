tcga_datasets_df = maftools::tcgaAvailable() %>% dplyr::mutate(Imported = logical(nrow(.)))
tcga_datasets_df$Study_Name

moduleLoadTCGAUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(heading="TCGA",
          DT::dataTableOutput(outputId = ns("out_dt_tcga_available"), width = "100%") %>% shinycssloaders::withSpinner(proxy.height = "200px"), 
          shinyWidgets::multiInput(
            inputId = ns("in_multi_tcga"), 
            label = "TCGA cohorts",
            choices = NULL, 
            choiceNames = tcga_datasets_df$Study_Name,
            choiceValues = tcga_datasets_df$Study_Abbreviation,
            selected = NULL,
            width = "100%"
            ),
          shinyWidgets::actionBttn(inputId = ns("in_bttn_load_selected_cohorts"), label = "Load selected cohorts")
    ),
    
    shinyWidgets::panel(heading="Summary",
      mod_single_cohort_summary_tables_and_plots_ui(id = ns("loaded_mafstats"))
    )
  )
  }

moduleLoadTCGAServer <- function(id, optional_argument){
  moduleServer(id,
    function(input, output, session){
      
      output$out_dt_tcga_available <- DT::renderDataTable({ tcga_datasets_df }, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
      
      tcga_maf_loaded_maf <- eventReactive(eventExpr = input$in_bttn_load_selected_cohorts,{
        validate(need(!is.null(input$in_multi_tcga), message = "Please choose TCGA cohorts"))
        get_tcga_mafs(input$in_multi_tcga)
        })
      
      cohortName <- function() { return("TCGA_Cohort") } #Has to be a function since mod_single_cohort_summary_tables_and_plots_server is expecting a reactive
      mod_single_cohort_summary_tables_and_plots_server(id = "loaded_mafstats", maf = tcga_maf_loaded_maf, cohortName = cohortName)
  }
  )
}



#' @title Get TCGA MAFs
#'
#' @param study_name Abbreviation of TCGA dataset to install. To see options, run maftools::tcgaAvailable()
#'
#' @return A single MAF object. 
#'
#'
get_tcga_mafs <- function(study_name){
  tcga_maf <- maftools::tcgaLoad(input$in_multi_tcga, source = "Firehose") #Could add option to swti
  
  if (is.list(tcga_maf))
    tcga_maf <- maftools::merge_mafs(tcga_maf)
  
  #browser()
  return(tcga_maf)
}

# Copy in UI
# moduleLoadTCGAUI("some_id")

# Copy in server
# moduleLoadTCGAServer("some_id", optional_argument)
