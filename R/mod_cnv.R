#' cnv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cnv_ui <- function(id){
  ns <- NS(id)
  tagList(
    #mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_maf_dataset_wrapper"))
    #all_lesions.conf_XX.txt, amp_genes.conf_XX.txt, del_genes.conf_XX.txt and scores.gistic, where XX is the confidence level.
    
    # Step 1: Import Data -----------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 1: Select Dataset",
      mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_dataset_wrapper"), panel = FALSE) %>% shinycssloaders::withSpinner(proxy.height = "200px")
    ),
    icon_down_arrow(),br(),
    
    # Step 2: Select GISTIC directory -----------------------------------------
    shinyWidgets::panel(
      heading = HTML(paste0("Step 2: Select GISTIC directory   ", as.character(select_gistic_help_icon()), sep = " ")),
      mod_shinydir_import_ui(
        id = ns("mod_select_gistic_dir"), 
        title = "Gistic Output Directory", 
        label = "Select GISTIC folder", 
        buttonType = "primary"
      ),
      
      wellPanel(textOutput(outputId = ns("out_text_directory"))),
      htmlOutput(outputId = ns("out_dir_looks_like_gistic"))
    ),
    icon_down_arrow(),br(),

    # Step 3: Check automatic identification of GISTIC files ------------------
    shinyWidgets::panel(
      heading = HTML(paste0("Step 3: Check automatic identification of required gistic files is correct", "      ", gistic_help_icon())),
      fluidRow(
        shinyWidgets::pickerInput(inputId = ns("in_pick_lesions"), label = "All Lesions", choices = NULL) %>% col_3(),
        shinyWidgets::pickerInput(inputId = ns("in_pick_amp"), label = "Amplified Genes", choices = NULL) %>% col_3(),
        shinyWidgets::pickerInput(inputId = ns("in_pick_del"), label = "Deleted Genes", choices = NULL) %>% col_3(),
        shinyWidgets::pickerInput(inputId = ns("in_pick_scores"), label = "Scores", choices = NULL) %>% col_3()
        
      )
    ),
    icon_down_arrow(),br(),

    # Step 4: Configure Analysis ----------------------------------------------
    shinyWidgets::panel(
      heading = "Step 4: Configure Analysis",
      fluidRow(
      shinyWidgets::awesomeCheckbox(inputId = ns("in_check_is_tcga"), label = "Data is from TCGA", value = FALSE) %>% 
        bsplus::bs_embed_tooltip(title = "Is the GISTIC data from a TCGA project? If so, we truncate the Tumor_Sample_Barcodes to patient level rather than sequencing_run level",placement = "top") %>%
         col_2(),
      shinyWidgets::awesomeRadio(inputId = ns("in_check_cn_level"), label = HTML(paste0("cnLevel", icon(""))), choices = c("all", "deep", "shallow"), selected = "all", inline = TRUE) %>%
        bsplus::bs_embed_tooltip(title = "Level of CN changes to use. Can be 'all', 'deep' or 'shallow'. Default uses all i.e, genes with both 'shallow' or 'deep' CN changes", placement="top") %>% 
        col_4() 
      )
    ),
    icon_down_arrow(),br(),
    # Step 5: Ensure Your Variant Dataset Sample Names Match Your CNV (GISTIC) Sample Names ------------------------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 5: Ensure your SNV vs CNV (GISTIC) sample names match",
      plotOutput(outputId = ns("out_plot_sample_name_overlap"), height = "300px", width="auto")
    ),
    icon_down_arrow(),br(),
    

    # Step 6: Analyse / Visualise ------------------------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 6: Choose Analysis / Visualisation",
      tabsetPanel(
        tabPanel(title = "Genome Plot", mod_plot_gistic_genome_ui(ns("mod_plot_genome"))),
        tabPanel(title = "Oncoplot", mod_plot_gistic_oncoplot_ui(ns("mod_plot_oncoplot"))),
        tabPanel(title = "Oncoplot Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_oncoplot_data"))),
        tabPanel(title = "Gene Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_gene_summary"))),
        tabPanel(title = "Sample Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_sample_summary"))),
        tabPanel(title = "Cytoband Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_cytoband_summary")))
      ),
      br()
    )
  )
}

#' cnv Server Functions
#'
#' @noRd 
mod_cnv_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # ReactiveVal ------------------------------------------------------------
    directory_looks_like_gistic <- reactiveVal(FALSE)
    
    
    
    # Step 1: Import Data -----------------------------------------------------
    maf_dataset_wrapper <- mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server(id = "mod_select_dataset_wrapper", maf_data_pool = maf_data_pool)
    maf_dataset_wrapper_validated <- reactive({ validate(need(!is.null(maf_dataset_wrapper()),message = "Loading ..." )); return(maf_dataset_wrapper()) })
    observe({ maf_dataset_wrapper_validated() ; maf() })
    maf <- reactive({ maf_dataset_wrapper_validated()$loaded_data })
    
    gistic_folder <- mod_shinydir_import_server(id = "mod_select_gistic_dir")
    gistic_folder_validated <- reactive({ validate(need(!is.null(gistic_folder()) && length(gistic_folder()) > 0,message = "Please select a valid gistic folder" )); return(gistic_folder()) })
    
    
    # Output Gistic Folder Path -----------------------------------------------
    output$out_text_directory <- renderText({ gistic_folder_validated() })
    
    #Look Inside the folder and grab the results ---------------------------------------------
    observeEvent(gistic_folder_validated(), {
      # browser()
      contents.v <- dir(gistic_folder_validated(), recursive = FALSE, full.names = FALSE)
      selected_lesions <- dir(gistic_folder_validated(), pattern = "all_lesions.conf_..\\.txt", recursive = FALSE, full.names = FALSE)
      amplified_genes <- dir(gistic_folder_validated(), pattern = "amp_genes.conf_..\\.txt", recursive = FALSE, full.names = FALSE)
      deleted_genes <- dir(gistic_folder_validated(), pattern = "del_genes.conf_..\\.txt", recursive = FALSE, full.names = FALSE)
      scores <- dir(gistic_folder_validated(), pattern = "scores.gistic", recursive = FALSE, full.names = FALSE)
      
      if(length(selected_lesions) > 0 && length(amplified_genes) > 0 && length(deleted_genes) > 0 && length(scores) > 0)
        directory_looks_like_gistic(TRUE)
      else 
        directory_looks_like_gistic(FALSE)
      
      
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_lesions", choices = contents.v, selected = dplyr::first(selected_lesions))
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_amp", choices = contents.v, selected = dplyr::first(amplified_genes))
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_del", choices = contents.v, selected = dplyr::first(deleted_genes))
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_scores", choices = contents.v, selected = dplyr::first(scores))
    })
    
    
    # Check if folder looks like gistic ---------------------------------------------------
    output$out_dir_looks_like_gistic <- renderText({
      if(is.null(gistic_folder()) || length(gistic_folder()) < 1) return(NULL)
      
      if(directory_looks_like_gistic()){
        p("The folder you picked looks like GISTIC output!", class="alert-success", gistic_check_circle()) %>% as.character() %>% return()
      }
      else
        p("Current folder does not look like GISTIC output! If you just renamed the gistic files, then continue (specify appropriate files in step 3). Otherwise, please pause and make sure the folder was generated by GISTIC", class="alert-danger", gistic_bad_path()) %>% as.character() %>% return()
    })
    
    
    # Derive paths for each file---------------------------------------------------
    lesion_path <- reactive({
      #browser()
      validate(need(!is.null(input$in_pick_lesions) && length(input$in_pick_lesions) > 0, message = "Please select a lesions file (all_lesions.conf_[threshold].txt)"))
      paste0(gistic_folder_validated(), "/", input$in_pick_lesions) %>% return()
      # else
      #   return(NULL)
    })
    amp_path <- reactive({
      validate(need(!is.null(input$in_pick_amp) && length(input$in_pick_amp) > 0, message = "Please select an amplified genes file (amp_genes.conf_[threshold].txt)"))
      paste0(gistic_folder_validated(), "/", input$in_pick_amp) %>% return()
      # else
      #   return(NULL)
    })
    del_path <- reactive({
      validate(need(!is.null(input$in_pick_del) && length(input$in_pick_del) > 0, message = "Please select a deleted genes file (del_genes.conf_[threshold].txt)"))
      paste0(gistic_folder_validated(), "/", input$in_pick_del) %>% return()
      # else
      #   return(NULL)
    })
    scores_path <- reactive({
      # browser()
      validate(need(!is.null(input$in_pick_scores) && length(input$in_pick_scores) > 0, message = "Please select a gistic.scores file (scores.gistic)"))
      paste0(gistic_folder_validated(), "/", input$in_pick_scores) %>% return()
      # else
      #   return(NULL)
    })
    
    # Create gistic object -------------------------------------------------
    gistic <- reactive({ 
      gistic_folder_validated() #Just here to trigger a validation message if no gistic folder is selected
      tryCatch(
        expr = { 
          maftools::readGistic(
            gisticAllLesionsFile = lesion_path(),
            gisticAmpGenesFile = amp_path(), 
            gisticDelGenesFile = del_path(), 
            gisticScoresFile = scores_path(), 
            isTCGA = input$in_check_is_tcga,
            cnLevel = input$in_check_cn_level,
          ) %>%
            return()
        },
        error = function(err){
          message("ERROR")
          err=as.character(err)
          validate(paste0("Problem with input files: \n\t", err))
          return(NULL)
        },
        warn = function(warn){
          message("WARNING")
          warn=as.character(warn)
          validate(paste0("Problem with input files: \n\t", warn))
          return(NULL)
        }
      )
    })
    

    # Sample Name Overlap -----------------------------------------------------

    output$out_plot_sample_name_overlap <- renderPlot({
      maftools_maf_and_gistic_sample_name_overlap_venn(maf = maf(), gistic = gistic())
    })
    
    
    # Summaries ---------------------------------------------------------------
    cytoband_summary_df <- reactive({ validate(need(!is.null(gistic()), message = "Waiting for valid gistic")); maftools::getCytobandSummary(gistic()) })
    sample_summary_df <- reactive({ validate(need(!is.null(gistic()), message = "Waiting for valid gistic")); maftools::getSampleSummary(gistic()) })
    gene_summary_df <- reactive({ validate(need(!is.null(gistic()), message = "Waiting for valid gistic")); maftools::getGeneSummary(gistic()) })
    oncoplot_data_df <- reactive({ validate(need(!is.null(gistic()), message = "Waiting for valid gistic")); gistic()@data %>% type.convert() })
    
    # Analyses -----------------------------------------------------------------
    output$out_dt_cytoband_summary <- mod_render_downloadabledataframe_server(id = "mod_downloadable_df_cytoband_summary", tabular_data_object = cytoband_summary_df, basename = "GISTIC_Cytoband_Summary")
    output$out_dt_sample_summary <- mod_render_downloadabledataframe_server(id = "mod_downloadable_df_sample_summary", tabular_data_object =  sample_summary_df, "GISTIC_Sample_Summary")
    output$out_dt_gene_summary <- mod_render_downloadabledataframe_server(id = "mod_downloadable_df_gene_summary", tabular_data_object =  gene_summary_df, "GISTIC_Gene_Summary")
    #output$out_dt_oncoplot_summary <-
    mod_render_downloadabledataframe_server(id = "mod_downloadable_df_oncoplot_data", tabular_data_object = oncoplot_data_df, basename = "GISTIC_Oncoplot_Data")
    
    mod_plot_gistic_genome_server("mod_plot_genome", gistic=gistic, maf=maf)
    mod_plot_gistic_oncoplot_server("mod_plot_oncoplot", gistic=gistic, maf=maf)
    
  })
}




# Bonus Functions ---------------------------------------------------------

select_gistic_help_icon <- function(){
  
  help_message = div(
      p(
        "Select the folder in which GISTIC files were output. ",
      "If you have a non-GISTIC seg file, consider running GISTIC 2.0 on the ",
      tags$a(target="_blank", "GenePattern", href="https://cloud.genepattern.org/gp/pages/index.jsf?lsid=urn:lsid:broad.mit.edu:cancer.software.genepattern.module.analysis:00125:6.15.28"),
      " platform"
      )
    )
    icon(name = "info-circle", style="margin-left: 10px;") %>%
      bsplus::bs_embed_tooltip(title = HTML(as.character(help_message)), placement = "top", html="true") %>%
      return()
}


gistic_help_icon <- function() {
  gistic_help <- 
    div(
      p("The gistic directory should contain the following files:"),
      hr(),
      p(tags$strong("all_lesions.conf_[threshold].txt")),
      hr(),
      p(tags$strong("amp_genes.conf_[threshold].txt")),
      hr(),
      p(tags$strong("del_genes.conf_[threshold].txt")),
      hr(),
      p(tags$strong("scores.gistic")),
      hr()
    )
  
  icon(name = "info-circle", style="margin-left: 10px;") %>% 
    bsplus::bs_embed_tooltip(title = HTML(as.character(gistic_help)), placement = "top", html="true") %>%
    return()
  #  )
}

gistic_check_circle <- function() {
  icon("check-circle") %>% 
    bsplus::bs_embed_tooltip(title = "The folder you selected has everything you need!", placement = "right")
}

gistic_bad_path <- function() {
  icon("exclamation-triangle") %>% 
    bsplus::bs_embed_tooltip(title = "The folder you selected doesn't have the files we'd expect in the output of a GISTIC run!", placement = "right")
}


maftools_maf_and_gistic_sample_name_overlap_venn <- function(maf, gistic) {
  assertthat::assert_that(utilitybelt::class_is(gistic, "GISTIC"))
  
  maf_sample_names <- maf %>% 
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  gistic_sample_names <- gistic %>%
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  ggVennDiagram::ggVennDiagram(list( "SNV" = maf_sample_names, "CNV" = gistic_sample_names)) +
    ggplot2::ggtitle("Tumor Sample Barcode Overlap") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"))
}

maftools_maf_and_gistic_random_sample_names <- function(maf, gistic) {
  assertthat::assert_that(utilitybelt::class_is(gistic, "GISTIC"))
  
  maf_sample_names <- maf %>% 
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  gistic_sample_names <- gistic %>%
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  if(length(maf_sample_names) == 0 || length(gistic_sample_names) == 0){
   message("[maftools_maf_and_gistic_random_sample_names] either maf_sample_names or gistic_sample_names are empty. Returning NULL") 
    return(NULL)
  }

  dplyr::tibble(
    RANDOM_SAMPLE_NAMES_IN_DATASET = maf_sample_names[runif(n = 5, min = 1, max = length(maf_sample_names))],
    RANDOM_SAMPLE_NAMES_IN_GISTIC =  gistic_sample_names[runif(n = 5, min = 1, max = length(gistic_sample_names))]
   )
}

## To be copied in the UI
# mod_cnv_ui("cnv_ui_1")

## To be copied in the server
# mod_cnv_server("cnv_ui_1")
