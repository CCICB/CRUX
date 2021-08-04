#' mutational_signatures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mutational_signatures_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::panel(
      heading = "Step 1: Select Dataset",
      mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_maf"), panel = FALSE)
    ),
    
    icon_down_arrow(), br(),
    
    shinyWidgets::panel(
      heading = "Step 2: Select Mutalisk Directory: ",
      wellPanel("You can generate this folder as follows: ",
                tags$ol(
                  tags$li("Go to ", tags$strong("External Tools => Export Data for Mutalisk")),
                  tags$li("Follow instructions to run mutational signature analysis"),
                  tags$li("Once the results are ready, Select",  tags$strong("Mutational Signature (Best only)"), " then click:", tags$strong("Get the selected result for all samples at once")),
                  tags$li("Unzip the file, and then select the directory using the button below")
                )
                ),
      mod_shinydir_import_ui(id = ns("in_choose_dir"), label = "Choose Mutalisk Directory", title = "Choose Mutalisk Directory"),
    ),
    
    shinyWidgets::panel(
      heading = "Step 3: Check Sample IDs match",
      uiOutput(outputId = ns("out_ui_samples_match"))
    ), 
    
    icon_down_arrow(), br(),
    
    shinyWidgets::panel(
      heading = "Step 4: Review Tabular Data",
      mod_render_downloadabledataframe_ui(ns("mutalisk_df")),
      br()
    ),
    
    icon_down_arrow(), br(),
    
    shinyWidgets::panel(
      heading="Step 5: Visualise Signature Contributions",
      mod_plot_mutational_signatures_ui(ns("mod_plot_mutsig"))
    )
  )
}

#' mutational_signatures Server Functions
#'
#' @noRd
mod_mutational_signatures_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Get dataset
    maf_dataset_wrapper <- mod_select_maf_dataset_wrapper_server(id = "mod_select_maf", maf_data_pool = maf_data_pool)
    
    # Get maf
    maf <- reactive({
      validate(need(!is.null(maf_dataset_wrapper()), message = "Please select a dataset"))
      maf_dataset_wrapper()$loaded_data
    })
    
    # Get sample level metadata
    sample_metadata_df <- reactive({
      maftools::getClinicalData(maf()) %>%
        dplyr::rename(SampleID = Tumor_Sample_Barcode)
    })
    
    # Get path to mutalisk directory
    mutalisk_dir <- mod_shinydir_import_server(id="in_choose_dir")
    
    # Parse directory contents and create a dataframe
    mutalisk_df <- reactive({
      validate(need(file.exists(mutalisk_dir()), message = "Please select a mutalisk output directory"))
      tryCatch({mutalisk::mutalisk_best_signature_directory_to_dataframe(directory = mutalisk_dir(), metadata = sample_metadata_df())
      },
      error=function(e){
        m = "Failed to parse mutalisk files. Are you sure the directory you selected was mutalisk output?"
        #error_message = paste0(as.character(e), collapse = ";")
        #full_error_message = paste(m, error_message, sep="\n")
        validate(m)
      }
      )
    })
    
    # Check samples in selected dataset match whats in mutalisk_dir
    dataset_matches_directory <- reactive({
      mutalisk_samples=unique(mutalisk_df()$SampleID)
      dataset_samples = unique(maftools::getSampleSummary(maf())[["Tumor_Sample_Barcode"]])
      if (all(mutalisk_samples %in% dataset_samples)){
        return(TRUE)
      }
      else 
        return(FALSE)
    })
    
    
    output$out_ui_samples_match <- renderUI({
      if(dataset_matches_directory()){
        html_alert("Sample IDs in mutalisk folder are all represented in the selcted dataset!",status = "success")
      }
      else{
        html_alert(paste0("Not all samples in dataset have mutational profile information. Are you sure you've selected the dataset you ran mutalisk on?"),status = "danger")
      }
    })
    
    # Render DataTable
    mod_render_downloadabledataframe_server("mutalisk_df", tabular_data_object = mutalisk_df)
    
    #Plot Mutsig
    mod_plot_mutational_signatures_server(id = "mod_plot_mutsig", mutalisk_df = mutalisk_df)
  })
}