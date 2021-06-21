#' select_dataset_from_maf_data_pool_pickerinput_and_return_maf_and_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @inheritParams mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_ui
#'
#' @importFrom shiny NS tagList 
mod_select_maf_dataset_wrapper_ui <- function(id, panel=TRUE){
  ns <- NS(id)
  tagList(
    mod_select_dataset_from_maf_data_pool_pickerinput_ui(ns("in_picker_dataset"), panel=panel) %>% shinycssloaders::withSpinner(proxy.height = "80px"),
    shinyWidgets::awesomeCheckbox(inputId = ns("in_check_filter_dubious_genes"), label = "Filter Dubious Genes", value = FALSE)
  )
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
  #browser()
  utilitybeltshiny::assert_reactive(maf_data_pool)

  moduleServer( id, function(input, output, session){
    #browser()
    ns <- session$ns
    selected_dataset_unique_name <- reactive({
      mod_select_dataset_from_maf_data_pool_pickerinput_server(id = "in_picker_dataset", maf_data_pool = maf_data_pool, label=label)()
    })

    #outputOptions(output, "in_picker_dataset", suspendWhenHidden = FALSE)

    maf_dataset_wrapper <- eventReactive(selected_dataset_unique_name(), { #Run only when pickerinput selected dataset changes changes
      isolate({
        #Step 0: Validate
        validate(need(!is.null(selected_dataset_unique_name()), message = "Please Select a Dataset"))

        #Step 1: Load data and update maf_data_pool
        new_data_pool <- maf_data_pool_robust_load(maf_data_pool(), selected_dataset_unique_name())
        maf_data_pool(new_data_pool)

        #Step 2: Grab Data Wrapper
        maf_dataset_wrapper_ = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = isolate(maf_data_pool()), unique_name = selected_dataset_unique_name())
        
        return(maf_dataset_wrapper_)
      })
    })

    # Remove dubious genes from maf if option is selected
    maf_dataset_wrapper_final <- reactive({
      if(input$in_check_filter_dubious_genes){
        bla <- maf_data_set_wrapper_remove_dubious_genes(maf_dataset_wrapper())
        return(bla)
      }
      else
        return(maf_dataset_wrapper())
      })
      
    
    return(maf_dataset_wrapper_final)

  })
}


conditionally_render_in_panel <- function(ui, condition, heading){
  if(condition)
    shinyWidgets::panel(heading = heading, ui) %>% return()
  else 
    ui %>% return()
}

mod_select_maf_dataset_wrapper2_ui <- function(id, panel = FALSE, heading = "Select Dataset"){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::pickerInput(inputId = ns("in_pick_dataset"), label = "Select Dataset", choices = NULL, multiple = FALSE, width = "100%", options  = shinyWidgets::pickerOptions(liveSearch = TRUE)) %>%
      conditionally_render_in_panel(condition = panel, heading = heading)
    
  )
}


mod_select_maf_dataset_wrapper2_server <- function(id, maf_data_pool, label = "Dataset"){
  #browser()
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer(id, function(input, output, session){
    #browser()
    ns <- session$ns
    
    maf_data_pool_df <- reactive({ maf_data_pool_to_dataframe(maf_data_pool()) })
    
    unique_dataset_names <- reactive({ 
      maf_data_pool_df()[["unique_name"]]
    } )
    
    unique_dataset_names_badge <- reactive({
      unique_dataset_names() %>%
        gsub("(TCGA_)|(PCAWG_)", "", .) %>%
        paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>")
    })
    
    
    display_names <- reactive({ 
      maf_data_pool_df()[["display_name"]] %>% 
        gsub("_", " ", .) 
    })
    
    data_sources <- reactive({
      data_sources_vec <- maf_data_pool_df()[["name_of_data_source"]][match(unique_dataset_names(), maf_data_pool_df()[["unique_name"]])]
      data_sources_vec <- paste0("<span class='label label-default' style='margin-left: 5px; font-size: xx-small' >",data_sources_vec, "</span>")
      return(data_sources_vec)
    })
    
    observeEvent(unique_dataset_names(), isolate({
    #observeEvent({unique_dataset_names(); isolate({
      #message("Unique_names = ", length(unique_dataset_names()))
      #message("Selected Dataset = ", input$in_pick_dataset)
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "in_pick_dataset", 
        selected = input$in_pick_dataset, 
        choices = unique_dataset_names(),
        choicesOpt = list(
          content = paste0(
                      display_names(),
                      unique_dataset_names_badge(),
                      data_sources()
                    )
        ))
    }))
    
    
    maf_dataset_wrapper <- reactive({
      #Step 0: Validate 
      validate(need(!is.null(input$in_pick_dataset), message = "Please Select a Dataset"))
      
      isolate({
        #Step 1: Load data and update maf_data_pool
        new_data_pool <- maf_data_pool_robust_load(maf_data_pool(), input$in_pick_dataset)
        maf_data_pool(new_data_pool)
        
        #Step 2: Grab Data Wrapper
        maf_dataset_wrapper_ = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = input$in_pick_dataset)
      })
      
      return(maf_dataset_wrapper_)
    })
    
    return(maf_dataset_wrapper)
    
  })
}

## To be copied in the UI
# mod_select_maf_dataset_wrapper_ui("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")

## To be copied in the server
# mod_select_maf_dataset_wrapper_server("select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui_1")
