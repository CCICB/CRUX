#' merge UI Function
#'
#' @description A shiny Module. Merges 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_merge_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = ns("merge_dataset_ui"),
    
      # Select Datasets ---------------------------------------------------------
      shinyWidgets::panel(
        heading = "Step 1: Select Datasets to Merge",
        mod_select_maf_datasets_wrapper_ui(id=ns("mod_select_datasets_return_list_of_maf_dataset_wrappers")),
      ), icon_down_arrow(break_after = TRUE),
      # shinyWidgets::panel(
      #   html_alert("Please note merging of datasets will not merge their rnaseq data, the new maf_data_wrapper will have no attached RNAseq data (feature will be added soon)", status = "info")
      #   ),
      # 
      # Get Dataset Info ----------------------------------------------------------------
      shinyWidgets::panel(
        heading = "Step 2: Review Operation",
        wellPanel(htmlOutput(ns("out_text_datasets_to_merge"))),
        
        shinyWidgets::panel(
          heading = "Tumor Sample Barcode Overlap",
          plotOutput(outputId = ns("out_plot_tsb_overlap")) %>% shinycssloaders::withSpinner(proxy.height = "200px")
        )
        ),
      icon_down_arrow(break_after = TRUE),
      
      # Summary of Merged Maf ---------------------------------------------------
      mod_single_cohort_summary_tables_ui(id = ns("mod_merged_maf_summary"), panel_heading = "Step 3: Review Dataset Summary"),
      icon_down_arrow(break_after = TRUE),
  
      # Add to Data Pool --------------------------------------------------------
      shinyWidgets::panel(
        heading = "Step 4: Add to Data Pool",
        mod_data_import_step2_ui(id = ns("mod_set_metadata")),
        dipsaus::actionButtonStyled(inputId = ns("in_bttn_add_to_data_pool"), label = "Add to Data Pool", disabled=TRUE, type = 'danger')
      )
    )
  )
}
    
#' merge Server Functions
#'
#' @noRd 
mod_merge_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Select Datasets ---------------------------------------------------------
    list_of_maf_dataset_wrappers <- mod_select_maf_datasets_wrapper_server(id="mod_select_datasets_return_list_of_maf_dataset_wrappers", maf_data_pool = maf_data_pool)


    
    # Get Dataset Info ----------------------------------------------------------------
    list_of_mafs <- reactive({
      validate(need(!is.null(list_of_maf_dataset_wrappers()), message = "Please Select Datasets to Merge"))
      purrr::map(list_of_maf_dataset_wrappers(), function(maf_dataset_wrapper) return(maf_dataset_wrapper$loaded_data))
    })
    
    list_of_tsbs <- reactive({
      validate(need(!is.null(list_of_mafs()), message = "Please Select Datasets to Merge"))
      tsb.ls = purrr::map(list_of_mafs(), function(maf) { sample_data=maftools::getSampleSummary(maf); sample_data[["Tumor_Sample_Barcode"]] } )
      names(tsb.ls) <- short_names.v()
      return(tsb.ls)
    })
    
    
    short_names.v <- reactive({
      validate(need(!is.null(list_of_maf_dataset_wrappers()), message = "Please Select Datasets to Merge"))
      purrr::map_chr(list_of_maf_dataset_wrappers(), function(maf_dataset_wrapper) return(maf_dataset_wrapper$short_name))
    })
    
    
    data_sources.v <- reactive({
      validate(need(!is.null(list_of_maf_dataset_wrappers()), message = "Please Select Datasets to Merge"))
      purrr::map_chr(list_of_maf_dataset_wrappers(), function(maf_dataset_wrapper) return(maf_dataset_wrapper$name_of_data_source))
    })
    
    unique_sources.v <- reactive({
      data_sources.v() %>% unique()
    })
    
    display_names.v <- reactive({
      validate(need(!is.null(list_of_maf_dataset_wrappers()), message = "Please Select Datasets to Merge"))
      purrr::map_chr(list_of_maf_dataset_wrappers(), function(maf_dataset_wrapper) return(maf_dataset_wrapper$display_name))
    })
    
    
    # Render Output
    output$out_text_datasets_to_merge <- renderText({  
      validate(need(!is.null(short_names.v()), message = "Please Select Datasets to Merge"))
      paste0(strong("Merged Datasets: "), paste0(short_names.v(), collapse=",")) 
      })
    
    output$out_plot_tsb_overlap <- renderPlot({ 
      #browser()
      if(length(list_of_tsbs()) <=4)
        ggvenn::ggvenn(list_of_tsbs()) %>% return()
      else
        UpSetR::upset(UpSetR::fromList(list_of_tsbs()), order.by = "freq") %>% return()
      })
    
    
     # Merged Datasets ---------------------------------------------------
    merged_maf <- reactive({ 
      validate(need(!is.null(list_of_mafs()), message = "Please Select Datasets to Merge")); 
      maftools::merge_mafs(list_of_mafs()) %>%
        maftools_fix_clinical_data_types()
    })
    
    # Summary of Merged Maf ---------------------------------------------------
    output$dt <- renderDataTable({
      validate(need(!is.null(merged_maf()), message = "Please Run Merge By Clicking Select"))
      maftools::getClinicalData(merged_maf())
    })
    
    mod_single_cohort_summary_tables_server(id = "mod_merged_maf_summary", maf = merged_maf)
    

    # Metadata defaults -------------------------------------------------------
    default_data_source <- reactive({
      paste0(unique_sources.v(), collapse=",")
      })
    
    default_description <- reactive({
      paste0("Merge of datasets: ", paste0(display_names.v() , collapse=","))
    })
    
    default_short_name <- reactive({
      paste0(short_names.v(), collapse=",")
      })
    
    default_display_name <- reactive({
      paste0("(merged) ", paste0(display_names.v(), collapse=","))
      })

    
    # Add to data pool --------------------------------------------------------
    metadata=mod_data_import_step2_server(id = "mod_set_metadata", default_data_source =  default_data_source, default_display_name = default_display_name, default_description = default_description, default_short_name = default_short_name)
    
    observeEvent(metadata(), {
      if(!is.null(merged_maf()) && metadata()$all_valid ==TRUE){
        dipsaus::updateActionButtonStyled(session = session, inputId = "in_bttn_add_to_data_pool", type = "success", disabled = FALSE)
      }
      else
        dipsaus::updateActionButtonStyled(session = session, inputId = "in_bttn_add_to_data_pool", type = "danger", disabled = TRUE)
      })
    
    
    observeEvent( input$in_bttn_add_to_data_pool, {
      
      isolate({
        validate(need(metadata()$all_valid == TRUE, message = "Please import valid metadata"))
        new_maf_dataset_wrapper = new_maf_dataset_wrapper(
          maf_data_pool = maf_data_pool(),
          display_name = metadata()$display_name,
          short_name = metadata()$short_name,
          clinical_data = maftools::getClinicalData(merged_maf()),
          name_of_data_source = metadata()$data_source,
          data_description = metadata()$description,
          unique_name = maf_data_pool_make_name_unique(maf_data_pool = maf_data_pool(), name = metadata()$display_name),
          is_dataset_downloadable = FALSE,
          is_dataset_loadable = FALSE,
          loaded_data = merged_maf(),
          start_status = "ready",
          function_to_download_data = function(x){NULL},
          function_to_load_data = function(x) {NULL},
          local_path_to_data = "NONE", 
          datatype_of_stored_object = "NA",
          derived_from = NA, # Should be maf_dataset_wrapper
          number_of_samples = maftools_number_of_samples(merged_maf())
        )
        
        new_maf_data_pool = maf_data_pool_add_dataset(maf_data_pool = maf_data_pool(), maf_dataset_wrapper = new_maf_dataset_wrapper)
        
        #Update maf_data_pool
        maf_data_pool(new_maf_data_pool)
        
        shinyWidgets::sendSweetAlert(session = session, title = "Success !!", text = "Dataset has been successfully imported! ", type = "success")
        shinyjs::reset(id = "merge_dataset_ui")
        dipsaus::updateActionButtonStyled(session = session, inputId = "in_bttn_add_to_data_pool", disabled = TRUE)
        
        #Reset Inputs
      })
    })
    
  })
}
## To be copied in the UI
# mod_merge_ui("merge_ui_1")
    
## To be copied in the server
# mod_merge_server("merge_ui_1")
