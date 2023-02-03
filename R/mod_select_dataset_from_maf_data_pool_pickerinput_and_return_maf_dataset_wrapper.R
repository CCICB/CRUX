
# New ---------------------------------------------------------------------
mod_select_maf_dataset_wrapper_ui <- function(id, panel = TRUE){
  ns <- NS(id)
  tagList(
    #shinyWidgets::panel(
    fluidRow(
      shinyWidgets::panel(status = "primary",
        shinyWidgets::pickerInput(
          inputId = ns("in_pick_dataset"),
          label = "Please select a dataset", 
          choices = NULL, 
          multiple = FALSE,
          selected = character(0), 
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE, 
            noneSelectedText = "Please select a dataset", 
            liveSearchNormalize = TRUE,
            size = 20,
          ), width = "100%"
          )) %>% col_12()
      ),
    fluidRow(
      shinyWidgets::panel(
          shinyWidgets::materialSwitch(
            inputId = ns("in_switch_filter_dubious_genes"), 
            label = "Filter Dubious Genes", 
            value = FALSE, inline = FALSE, width = "100%"
          )
          # shinyWidgets::materialSwitch(
          #   inputId = ns("in_switch_anonymise_samples"),
          #   label = "Anonymise Samples",
          #   value = FALSE, inline = FALSE, width = "100%"
          # ),
          ) %>% col_12()
      
    )
    
  )
}


sample_numbers_formatted <- function(maf_data_pool_df){
  maf_data_pool_df[["number_of_samples"]] %>%
  as.character() %>%
  ifelse(is.na(.), yes = "unknown", no = .) %>%
  paste0("N = ", .) %>%
  paste0("(", ., ")")
}

display_names_formatted <- function(maf_data_pool_df){
  maf_data_pool_df[["display_name"]] %>% 
  gsub("_", " ", .) 
}

short_dataset_names_badge <- function(maf_data_pool_df){
  maf_data_pool_df[["short_name"]] %>%
    gsub("(^TCGA_)|(^PCAWG_)", "", .) %>%
    paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>")
}

data_sources_formatted <- function(maf_data_pool_df){
  maf_data_pool_df[["name_of_data_source"]] %>%
    paste0("<span class='label label-default' style='margin-left: 5px; font-size: xx-small' >",., "</span>")  
}

#' Select Dataset, Return maf_dataset_wrapper
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_server
#' 
#' @description wraps mod_select_dataset_from_maf_data_pool_pickerinput_server.
#' Instead of simply returning a unique_name, this function will:
#' 
#' 
#' \enumerate{ 
#' \item Load the specified dataset into memory if required.
#' \item Update maf_data_pool (a reactiveVal) to indicate the dataset has been loaded.
#' \item Return the relevant maf_dataset_wrapper. See \strong{Accessing Properties} section for details.
#' }
#' 
#' @section Accessing Properties (Quick Reference):
#' 
#' \tabular{rrr}{
#' \strong{MAF object:} \tab \tab \code{maf_dataset_wrapper()$loaded_data} \cr
#' \strong{unique name:} \tab \tab \code{maf_dataset_wrapper()$unique_name} \cr
#' \strong{short name:} \tab \tab \code{maf_dataset_wrapper()$short_name} \cr
#' \strong{full name:} \tab \tab \code{maf_dataset_wrapper()$display_name} \cr 
#' \strong{source:} \tab \tab \code{maf_dataset_wrapper()$name_of_data_source} \cr 
#' }
#' 
#' 
#' See ?new_maf_dataset_wrapper for the full list of properties
#' 
#' 
#' 
#' @return maf_dataset_wrapper.
mod_select_maf_dataset_wrapper_server <- function(id, maf_data_pool, label = "Dataset"){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    

    # Update Cohort Choices ---------------------------------------------------
    prev_unique_dataset_names <- reactiveVal(NULL)
    
    # Will run every time maf_data_pool changes (even if a maf_dataset_wrapper$loaded_data slot changes),
    # but will only update the cohort list if a new cohort is added
    observeEvent(maf_data_pool(), isolate({
      validate(need(!is.null(maf_data_pool()), message = "Loading ... "))
      maf_data_pool_df <- maf_data_pool_to_dataframe(maf_data_pool())
      unique_dataset_names <- maf_data_pool_df[["unique_name"]]
      
      # Check new cohort added
      if(!identical(unique_dataset_names, prev_unique_dataset_names())){
        prev_unique_dataset_names(unique_dataset_names)
        
        # formatted elements for composing pickerInput content
        sample_numbers <- sample_numbers_formatted(maf_data_pool_df)
        display_names <- display_names_formatted(maf_data_pool_df)
        short_dataset_names <- short_dataset_names_badge(maf_data_pool_df)
        data_sources <- data_sources_formatted(maf_data_pool_df)
        
        # Update Pickerinput
        shinyWidgets::updatePickerInput(
          session = session, inputId = "in_pick_dataset", 
          selected = if (is.null(selected_dataset_unique_name())) character(0) else selected_dataset_unique_name(), 
          choices = unique_dataset_names,
          choicesOpt = list(
            content = paste0(
              display_names,
              " ",
              sample_numbers,
              short_dataset_names,
              data_sources
            )
          )
        )
      }
      #else
      #  message("maf_data_pool changed but unique dataset names remaing identical, not reloading pickerinputs")
    }))
    

    selected_dataset_unique_name <- reactive(input[["in_pick_dataset"]])
    
    
    # Load data and set maf_dataset_wrapper -----------------------------------
    maf_dataset_wrapper <- eventReactive(
      eventExpr = c(selected_dataset_unique_name(), input[["in_switch_filter_dubious_genes"]], input[["in_switch_anonymise_samples"]]), 
      
      valueExpr = isolate({
        #Step 0: Validate
        validate(need(!is.null(selected_dataset_unique_name()), message = "Please Select a Dataset"))
        
        # Step 1: Load data and update maf data pool
        new_data_pool <- maf_data_pool_robust_load(maf_data_pool(), unique_name = selected_dataset_unique_name())
        maf_data_pool(new_data_pool)
        
        # Step 2: Grab Data Wrapper
        maf_dataset_wrapper_ = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool =  maf_data_pool(), unique_name = selected_dataset_unique_name())
        
        # Step 3: Filter Out Dubious Genes if button is checked
        filter_dubious_genes <- input[["in_switch_filter_dubious_genes"]]
        if(filter_dubious_genes){
          maf_dataset_wrapper_ <- maf_data_set_wrapper_remove_dubious_genes(maf_dataset_wrapper_)
        }
        
        # # Step 4: Anonymise data  
        # anonymise_data <- input[["in_switch_anonymise_samples"]]
        # if(anonymise_data)
        #   maf_dataset_wrapper_ <- maf_data_set_wrapper_anonymise_maf(maf_dataset_wrapper_)
        
        
        # Step 5: Return dataset wrapper
        return(maf_dataset_wrapper_)
    }))
    
    return(maf_dataset_wrapper)
  })
}


## To be copied in the UI
# mod_select_maf_dataset_wrapper_ui("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")

## To be copied in the server
# mod_select_maf_dataset_wrapper_server("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")
