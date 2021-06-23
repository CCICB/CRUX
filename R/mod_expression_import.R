#' expression_analyis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_expression_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::panel("Expression data is of limited use in this version of CRUX. Currently, you can't do much more than export the data for use in the Xena Browser. Features will be added in future versions."),
    
    # Step 1: Select Dataset --------------------------------------------------
    mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_dataset"), panel = TRUE),
    
    icon_down_arrow(),br(),
    
    # Step 2: Check for existing expression data --------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 2: Check for existing RNA data",
      uiOutput(outputId = ns("out_ui_rna_status"))
    ),
    icon_down_arrow(),br(),
    
    # Step 3: Import expression data --------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 3: Import expression data",
      wellPanel("A tab or comma separated file containing the following columns: ",
                tags$ol(
                  tags$li("Tumor_Sample_Barcode (sample name)"),
                  tags$li("Hugo_Symbol (name of gene)"),
                  tags$li("TPM (Transcripts Per Million)"),
                ),
                "Column order doesn't matter but the file must have a header line containing Tumor_Sample_Barcode, Hugo_Symbol & TPM"
      ),
      hr(),
      mod_shinyfile_import_ui(id = ns("mod_shinyfile"), multiple = FALSE, title = "Import RNA data", label = "Import RNA data", buttonType = "primary", style = "width: 100%")
    ),
    
    icon_down_arrow(),br(),
    
    # Step 4: Check validity of expression file --------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 4: Check RNAseq file is valid",
      uiOutput(ns("out_ui_rna_is_valid"))
    ),
    
    icon_down_arrow(),br(),
    
    # Step 4: Ensure sample names match--------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 5: Check RNA and DNA sample names match",
      plotOutput(ns("out_plot_tsb_venn")) %>% shinycssloaders::withSpinner(),
      uiOutput(ns("out_ui_sample_id_overlap"))
    ),
    
    icon_down_arrow(),br(),
    
    shinyWidgets::panel(
      heading = "Step 6: Examine expression data",
      mod_render_downloadabledataframe_ui(ns("mod_downloadable_dataframe")),
      DT::DTOutput(ns("out_dt_rna")),
      textOutput(ns("tmp"))
    ),
    
    icon_down_arrow(),br(),
    
    shinyWidgets::panel(
      heading = "Step 7: Confirm",
      dipsaus::actionButtonStyled(inputId = ns("in_bttn_add_rna_to_data_pool"), label = "Add Expression Data", type = "warning", disabled = TRUE, width = "100%")
    )
  )
}

#' expression_analyis Server Functions
#'
#' @noRd 
mod_expression_import_server <- function(id, maf_data_pool){
  assertthat::assert_that(is.reactive(maf_data_pool))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Step 1: Select Dataset --------------------------------------------------
    maf_data_wrapper <- mod_select_maf_dataset_wrapper_server(id = "mod_select_dataset", maf_data_pool = maf_data_pool, label = "Step 1: Select Dataset")
    maf_data_wrapper_unique_name <- reactive({maf_data_wrapper()$unique_name})
    maf <- reactive({ maf_data_wrapper()$loaded_data })
    
    # Step 2: Summarise Dataset --------------------------------------------------
    has_rna_data <- reactive({
      maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = maf_data_wrapper_unique_name()) %>%
        maf_data_wrapper_has_rnaseq_data()
    })
    output$out_ui_rna_status <- renderUI({
      if (has_rna_data())
        html_alert(text = "RNA data found. If you import another file, existing data will be overwritten", status = "success") %>% return()
      else
        html_alert(text = "No valid RNA data found. Please import some", status =  "warning") %>% return()
    })
    
    # Step 3: Add RNA data to Dataset --------------------------------------------------
    rnaseq_filepath <- mod_shinyfile_import_server("mod_shinyfile")
    
    rnaseq_valid <- reactive({
      validate(need(length(rnaseq_filepath()) > 0, message = "Please select a file"))
      val = tryCatch(
        expr = { 
          read_rnaseq_file(rnaseq_filepath())
          return(TRUE)
        },
        error = function(err){
          message(as.character(err))
          return(c(FALSE) %>% setNames(as.character(err)) )
        }#,
        # warning = function(warn){
        #   return(c(FALSE) %>% setNames(as.character(warn)) )
        # }
      )
      return(val)
    })
    
    output$out_ui_rna_is_valid <- renderUI({ 
      if(rnaseq_valid()){
        html_alert("RNA file is as expected",status = "success") %>% return()
      } 
      else 
        html_alert(text = paste0("RNA file is not in the correct format.", names(rnaseq_valid())[1]),status = "danger") %>% return()
    })
    
    
    rnaseq_df <- reactive({
      validate(need(rnaseq_valid(), message = "Please import a valid RNA file"))
      read_rnaseq_file(rnaseq_filepath())
      # maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool(), unique_name = maf_data_wrapper_unique_name()) %>%
        # maf_data_wrapper_get_rnaseq_df()
    })
    
    
    # Sample Overlap ----------------------------------------------------------
    tsbs_RNA <- reactive({
      validate(need(!is.null(rnaseq_df()) && !is.null(maf()), message = "Please upload a valid RNA dataset"))
      RNA = rnaseq_df() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
    })
    
    tsbs_DNA <- reactive({
      validate(need(!is.null(rnaseq_df()) && !is.null(maf()), message = "Please upload a valid RNA dataset"))
      DNA = maf() %>% maftools::getSampleSummary() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
    })
    
    tsbs_CLINICAL <- reactive({
      validate(need(!is.null(rnaseq_df()) && !is.null(maf()), message = "Please upload a valid RNA dataset"))
      CLINICAL = maf() %>% maftools::getClinicalData() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
    })
    
    output$out_plot_tsb_venn <- renderPlot({
      validate(need(!is.null(rnaseq_df()) && !is.null(maf()), message = "Please upload a valid RNA dataset"))
      RNA = rnaseq_df() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
      DNA = maf() %>% maftools::getSampleSummary() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
      CLINICAL = maf() %>% maftools::getClinicalData() %>% dplyr::pull(Tumor_Sample_Barcode) %>% unique()
      ggVennDiagram::ggVennDiagram(list(
        RNA=RNA,
        DNA=DNA
        ))
      })
    
    output$out_ui_sample_id_overlap <- renderUI({
      if(sum(tsbs_RNA() %in% tsbs_DNA()) < 0.7){
        warning_message <- paste0(sum(tsbs_RNA() %in% tsbs_DNA())*100, "% of the samples in your RNA dataset are present in your Mutational dataset. Please ensure you've selected the right cohort and your Tumor_Sample_Barcodes match")
        html_alert(warning_message, status = "warning")
      }
    })
    
    # Render RNAseq dataframe -------------------------------------------------
    mod_render_downloadabledataframe_server("mod_downloadable_dataframe", tabular_data_object = rnaseq_df, basename = "rnaseq", message_if_tabular_data_is_null = "No RNAseq data found")
    
    
    observeEvent(rnaseq_valid(), isolate({
      if(rnaseq_valid())
        dipsaus::updateActionButtonStyled(session = session, "in_bttn_add_rna_to_data_pool", disabled = FALSE, type = "success")  
      else
        dipsaus::updateActionButtonStyled(session = session, "in_bttn_add_rna_to_data_pool", disabled = TRUE, type = "danger")  
      
      
      }))

    # Confirm Addition Button -------------------------------------------------
    observeEvent(input$in_bttn_add_rna_to_data_pool,isolate({
      dipsaus::updateActionButtonStyled(disabled = TRUE, label = "Waiting for data to load ... ",  session = session, inputId = "in_bttn_add_rna_to_data_pool")
        validate(need(length(rnaseq_filepath()) > 0, message = "Please select a file"))
        validate(need(!is.null(maf_data_wrapper_unique_name()), message = "Loading data"))
        validate(need(rnaseq_valid(), message = "RNAseqfile not valid"))
        
        message("adding RNA data to maf_data_pool", maf_data_wrapper_unique_name())
        
        maf_data_pool(
          maf_data_pool_add_rnaseq(maf_data_pool(), unique_name = maf_data_wrapper_unique_name(), rnaseq_path = rnaseq_filepath())
        )
        message("\nRNA data has been added")
        shinyWidgets::sendSweetAlert(session = session, title = "Success", type = "success", text = "Your expression data has been added. Please do not move or edit your expression file for the duration of your session.")
        dipsaus::updateActionButtonStyled(disabled = FALSE, label = "Add Expression Data", session = session, inputId = "in_bttn_add_rna_to_data_pool")
      }))
  })
}

## To be copied in the UI
# mod_expression_import_ui("expression_analyis_ui_1")

## To be copied in the server
# mod_expression_import_server("expression_analyis_ui_1")
