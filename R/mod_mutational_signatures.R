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
      heading = "Step 2: Select Mutalisk Files: ",
      wellPanel("You can generate these files as follows: ",
                tags$ol(
                  tags$li("Go to ", tags$strong("External Tools => Export Data for Mutalisk")),
                  tags$li("Follow instructions to run mutational signature analysis"),
                  tags$li("Once the results are ready, Select",  tags$strong("Mutational Signature (Best only)"), " then click:", tags$strong("Get the selected result for all samples at once")),
                  tags$li("Unzip the file, then click 'browse' below.  select all <strong>.txt</strong> files in the unzipped mutalisk folder")
                )
                ),
      fileInput(inputId = ns("in_mutalisk_files"), label = "Mutalisk Files", multiple = TRUE, accept = ".txt", width = "100%", placeholder = "Please select all Mutalisk Reports")
      #mod_shinydir_import_ui(id = ns("in_choose_dir"), label = "Choose Mutalisk Directory", title = "Choose Mutalisk Directory"),
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
    mutalisk_files <- reactive({
      validate(need(!is.null(input[["in_mutalisk_files"]]$datapath), message = "Please select mutalisk files"))
      return(input[["in_mutalisk_files"]]$datapath)
      })
    
    # Parse directory contents and create a dataframe
    mutalisk_df <- reactive({
      validate(need(!is.null(mutalisk_files()), message = "Please select mutalisk output files"))
      validate(need(!is.null(sample_metadata_df()), message = "Please select mutalisk output files"))
      
      
      df_mutalisk <- tryCatch({
          browser()
          df_mutalisk_ <- mutalisk::mutalisk_to_dataframe(mutalisk_files = mutalisk_files(), sample_names_from_file_contents = TRUE)
          df_mutalisk_ <- mutalisk::mutalisk_dataframe_add_metadata(df_mutalisk_, sample_metadata = sample_metadata_df())
          return(df_mutalisk_)
      },
      error=function(e){
        shinyWidgets::sendSweetAlert(
          session = session, 
          title = "Failed to Read Mutalisk Input",
          type = "error", text = tags$code(as.character(e))
        )
        #error_message = paste0(as.character(e), collapse = ";")
        #full_error_message = paste(m, error_message, sep="\n")
        validate(m)
      }
      )
      browser()
      return(df_mutalisk)
    })
    
    # Check samples in selected dataset match whats in mutalisk_dir
    samples_undescribed_by_mutalisk <- reactive({
      mutalisk_samples = unique(mutalisk_df()$SampleID)
      dataset_samples = unique(maftools::getSampleSummary(maf())[["Tumor_Sample_Barcode"]])
      dataset_samples[!dataset_samples %in% mutalisk_samples]
      })
    
    dataset_matches_mutalisk <- reactive({
      length(samples_undescribed_by_mutalisk) == 0
    })
    
    
    output$out_ui_samples_match <- renderUI({
      if(dataset_matches_mutalisk()){
        html_alert("Sample IDs in mutalisk folder are all represented in the selcted dataset!",status = "success")
      }
      else{
        html_alert(
          tags$span(
            "Not all samples in dataset have mutational profile information. Are you sure you've selected the dataset you ran mutalisk on?",
            "\nMissing mutalisk data for ", tags$strong(length(samples_undescribed_by_mutalisk()))," samples",
            ifelse(
              length(samples_undescribed_by_mutalisk()) < 10, 
              yes = paste0(samples_undescribed_by_mutalisk(), collapse = ","),
              no = ""
            ),
            collapes = "\n"),
          status = "danger")
      }
    })
    
    # Render DataTable
    mod_render_downloadabledataframe_server("mutalisk_df", tabular_data_object = mutalisk_df)
    
    #Plot Mutsig
    mod_plot_mutational_signatures_server(id = "mod_plot_mutsig", mutalisk_df = mutalisk_df)
  })
}