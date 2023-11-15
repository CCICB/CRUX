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
    ), icon_down_arrow(break_after = TRUE),
    
    shinyWidgets::panel(
      heading = "Step 3: Check Sample IDs match",
      #uiOutput(outputId = ns("out_ui_samples_match")),
      plotOutput(outputId = ns("out_plot_sample_overlap_venn"))
    ),
    
    icon_down_arrow(break_after = TRUE),
    
    shinyWidgets::panel(
     heading = "Step 4: Review Tabular Data",
     mod_render_downloadabledataframe_ui(ns("mutalisk_df")),
     br()
    ),

    icon_down_arrow(break_after = TRUE),

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
    
    # Get maf dataset wrapper
    maf_dataset_wrapper <- mod_select_maf_dataset_wrapper_server(id = "mod_select_maf", maf_data_pool = maf_data_pool)
    
    # Get maf
    maf <- reactive({
      #validate(need(!is.null(maf_dataset_wrapper()), message = "Please select a dataset"))
      maf_dataset_wrapper()$loaded_data
    })
    
    # Get sample level metadata
    sample_metadata_df <- reactive({
      
      if(is.null(maf()))
         return(NULL)
      else {
        maftools::getClinicalData(maf()) %>%
        dplyr::rename(SampleID = Tumor_Sample_Barcode)
      }
    })
    
    # Get paths to mutalisk files
    mutalisk_files <- reactive({
      validate(need(!is.null(input[["in_mutalisk_files"]]$datapath), message = "Please select mutalisk files"))
      return(input[["in_mutalisk_files"]]$datapath)
    })
    
    # Parse directory contents and create a dataframe
    mutalisk_df <- reactive({
      validate(need(!is.null(mutalisk_files()), message = "Please select mutalisk output files"))
      #validate(need(!is.null(sample_metadata_df()), message = "Please select a dataset output files"))
      
      
      tryCatch({
          df_mutalisk_ <- mutaliskRutils::mutalisk_to_dataframe(mutalisk_files = mutalisk_files(), sample_names_from_file_contents = TRUE)
          if(!is.null(sample_metadata_df())) df_mutalisk_ <- mutaliskRutils::mutalisk_dataframe_add_metadata(df_mutalisk_, sample_metadata_df())
          
          return(df_mutalisk_)
      },
      error=function(e){
        shinyWidgets::sendSweetAlert(
          session = session, 
          title = "Failed to Read Mutalisk Input",
          type = "error", text = tags$code(as.character(e))
        )
        return(NULL)
      }
      )
      return(NULL)
    })
    
    # Check samples in selected dataset match whats in mutalisk_dir
    mutalisk_samples = reactive({
      mutalisk_df()$SampleID
      })
    maf_dataset_samples = reactive({
      validate(need(!is.null(maf()), message = "No dataset selected in Step 1. All mutalisk samples will be visualised"))
      unique(maftools::getSampleSummary(maf())[["Tumor_Sample_Barcode"]])
      })
    
    samples_undescribed_by_mutalisk <- reactive({
      maf_dataset_samples()[!maf_dataset_samples() %in% mutalisk_samples()]
      })
    
    samples_described_by_both_mutalisk_and_maf <- reactive({
      intersect(maf_dataset_samples(), mutalisk_samples()) 
      })
    
    samples_undescribed_by_maf <- reactive({
      mutalisk_samples()[!mutalisk_samples() %in% maf_dataset_samples()]
    })
    
    dataset_matches_mutalisk <- reactive({
      length(samples_undescribed_by_mutalisk()) == 0 & length(samples_undescribed_by_maf()) == 0
    })
    
    output$out_plot_sample_overlap_venn <- renderPlot({
      ggvenn::ggvenn(list("Mutalisk" = mutalisk_samples(), "MAF" = maf_dataset_samples()))
      })
    
    output$out_ui_samples_match <- renderUI({
      
      if(dataset_matches_mutalisk()){
        html_alert("Perfect match between mutalisk samples and the selected dataset!",status = "success")
      }
      else{
        html_alert(
          tags$span(
            "Incomplete match between mutalisk samples and dataset samples. Only ", length(samples_described_by_both_mutalisk_and_maf()),"/",length(maf_dataset_samples()), " samples will will be analysed (those described in both CRUX dataset and mutalisk files)"),
          status = "danger")

      }
    })
    
    # Render DataTable
    mod_render_downloadabledataframe_server("mutalisk_df", tabular_data_object = mutalisk_df)
    
    #Plot Mutsig
    mod_plot_mutational_signatures_server(id = "mod_plot_mutsig", mutalisk_df = mutalisk_df)
  })
}
