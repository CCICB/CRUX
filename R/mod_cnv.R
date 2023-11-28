df_tcga_gistic <- TCGAgistic::tcga_gistic_available()
num_gistic_choices <- seq_len(nrow(df_tcga_gistic)) 
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
  
    
  
    # Step 1: Choose inbuilt VS custom -----------------------------------------
    shinyWidgets::panel(
      heading = "Step 1: Are you using inbuilt GISTIC data or importing your own?",
      shinyWidgets::radioGroupButtons(
        inputId = ns("in_bttn_preloaded_or_user"),
        label = NULL,
        choices = c("Inbuilt", "Custom"),
        justified = TRUE)
    ), icon_down_arrow(break_after = TRUE),
    
    # Step 1: Select GISTIC data -----------------------------------------
    shinyWidgets::panel(
      heading = HTML(paste0("Step 2: Select GISTIC (CNV) Data ", as.character(select_gistic_help_icon()), sep = " ")),
      
      tabsetPanel(
        id = ns("in_tabset"), 
        type = "hidden",
        tabPanelBody(
          value = "Inbuilt",
          
          fluidRow(
            shinyWidgets::pickerInput(
              inputId = ns("in_pick_gistic"), 
              label = "Select CNV dataset", 
              choices = seq_len(nrow(df_tcga_gistic)), width = "100%",
              options = list(title = "Please select a GISTIC dataset", `live-search`=TRUE),
              choicesOpt = list(
                content = paste0(
                  TCGAgistic::tcga_gistic_available()[[2]],
                  " ",
                  TCGAgistic::tcga_gistic_available()[[4]] %>% paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>"),
                  " ",
                  TCGAgistic::tcga_gistic_available()[[1]] %>% paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>"),
                  " ",
                  "TCGA" %>% paste0("<span class='label label-default' style='margin-left: 10px; font-size: xx-small' >",., "</span>")
                )
              )) %>% col_4(),
            shinydashboard::box(
              title = "CNV (GISTIC) datasets", width = "100%",
              "
              GISTIC2 is a toolkit that takes copynumber data for a cohort and identifies strong &/or recurrent amplifications/deletions.
              For TCGA datasets, use the inbuilt GISTIC datasets. Shallow/Deep/All refers to size of the CNVs to display.
              "
            ) %>% col_8()
          )
        ),
        tabPanelBody(
          value = "Custom",
          fluidRow(
          fileInput(inputId = ns("in_file_gistic"), label = "Select your GISTIC rds", width = "100%") %>% col_4(),
          shinydashboard::box(
            title = "Creating your own (GISTIC) datasets", width = "100%",
            "
            GISTIC2 is a toolkit that takes copynumber data for a cohort and identifies strong and/or recurrent amplifications/deletions.
            For custom datasets:
            Run GISTIC on th genepattern server -> convert to a CRUX RDS using CCICB/interchangeGUI -> Import Here.
            "
          ) %>% col_8()
          )
          
        ),
      )
    ),
    icon_down_arrow(),br(),
    
    # Step 1: Import Mutational Data -----------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 3: Select Dataset",
      mod_select_maf_dataset_wrapper_ui(id = ns("mod_select_dataset_wrapper"), panel = FALSE)
    ),
    icon_down_arrow(break_after = TRUE),
    
   
    
    # Step 5: Ensure Your Variant Dataset Sample Names Match Your CNV (GISTIC) Sample Names ------------------------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 4: Ensure your SNV vs CNV (GISTIC) sample names match",
      plotOutput(outputId = ns("out_plot_sample_name_overlap"), height = "300px", width="auto") %>% shinycssloaders::withSpinner()
    ),
    icon_down_arrow(break_after = TRUE),
    
    
    # Step 6: Analyse / Visualise ------------------------------------------------------------------
    shinyWidgets::panel(
      heading = "Step 5: Choose Analysis / Visualisation",
      tabsetPanel(
        tabPanel(title = "Genome Plot", mod_plot_gistic_genome_ui(ns("mod_plot_genome"))),
        tabPanel(title = "Oncoplot", mod_plot_gistic_oncoplot_ui(ns("mod_plot_oncoplot")))
        # tabPanel(title = "Oncoplot Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_oncoplot_data"))),
        # tabPanel(title = "Gene Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_gene_summary"))),
        # tabPanel(title = "Sample Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_sample_summary"))),
        # tabPanel(title = "Cytoband Summary", mod_render_downloadabledataframe_ui(id=ns("mod_downloadable_df_cytoband_summary")))
      ),
      br()
    )
  )
}

#' cnv Server Functions
#'
#' @noRd 
mod_cnv_server <- function(id, maf_data_pool){
  assertions::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # ReactiveVal ------------------------------------------------------------
    # directory_looks_like_gistic <- reactiveVal(FALSE)
    # 
    # 
    # 
    # Step 1: Import Data -----------------------------------------------------
    maf_dataset_wrapper <- mod_select_maf_dataset_wrapper_server(id = "mod_select_dataset_wrapper", maf_data_pool = maf_data_pool)
    maf_dataset_wrapper_validated <- reactive({ validate(need(!is.null(maf_dataset_wrapper()),message = "Please select a dataset ..." )); return(maf_dataset_wrapper()) })

    #observe({ maf_dataset_wrapper_validated() ; maf() })
    maf <- reactive({ maf_dataset_wrapper_validated()$loaded_data })

    use_inbuilt_gistic <- reactive({input[["in_bttn_preloaded_or_user"]] == "Inbuilt"})
    
    # Choose where to get your user input from
    observeEvent(use_inbuilt_gistic(), isolate({
      if(!use_inbuilt_gistic()){
        updateTabsetPanel(session = session, inputId = "in_tabset", selected = "Custom")
        #shinyjs::hide(id = input[["in_pick_gistic"]])
      }
      else if(use_inbuilt_gistic())
        updateTabsetPanel(session = session, inputId = "in_tabset", selected = "Inbuilt")
    }))
    
    

    # Select Gistic Files -----------------------------------------------------
    # output$out_text_directory <- renderText({
    #   input[['in_file_gistic_data']]$datapath
    #   })
    
    gistic_selected <- reactive({
      !is.null(input[["in_pick_gistic"]]) && nchar(input[["in_pick_gistic"]]) > 0
    })

    
    gistic <- reactive({
      
      # If we use inbuilt gistic
      if(use_inbuilt_gistic()){
        validate(need(gistic_selected(), message = "Please select a gistic dataset"))
        gistic_ <- tryCatch(
          expr = { 
            
            index <- as.numeric(input[["in_pick_gistic"]])
            cohort <- df_tcga_gistic[["Cohort"]][index]
            source <- df_tcga_gistic[["Source"]][index]
            cnLevel <- df_tcga_gistic[["CopyNumberLevel"]][index]
            
            gistic_ <- TCGAgistic::tcga_gistic_load(cohort = cohort, source = source, cnLevel = cnLevel, verbose = FALSE)
          },
          error = function(err){
            shinyWidgets::sendSweetAlert(session = session, title = "Failed to Read Gistic", text = err2html(err))
            validate("Failed to Read Gistic")
          },
          warning = function(warn){
            shinyWidgets::sendSweetAlert(session = session, title = "Failed to Read Gistic", text =  err2html(warn))
            validate("Failed to Read Gistic")
          }
          
        )
        return(gistic_)
      }
      # If we use user-supplied gistic
      else{
        validate(need(!is.null(input[["in_file_gistic"]]$datapath), message = "Please supply a valid GISTIC2 rds file"))
        
        gistic_ <- tryCatch(
         expr = { 
           g <- readRDS(file = input[["in_file_gistic"]]$datapath)
           if(!inherits(g, "GISTIC")) 
             shinyWidgets::sendSweetAlert(session = session, title = "Failed to Read Gistic", text = tags$span(tags$code("RDS file does not encode a GISTIC object.")))
         },
         error = function(err){
           shinyWidgets::sendSweetAlert(session = session, title = "Failed to Read Gistic", text = err2html(err))
           validate("Please supply a valid Gistic RDS")
         },
         warning = function(warn){
           shinyWidgets::sendSweetAlert(session = session, title = "Failed to Read Gistic", text = err2html(warn))
           validate("Please supply a valid Gistic RDS")
         }
       ) 
        return(gistic_)
      }
    })
  
    
    # Sample Name Overlap -----------------------------------------------------
    
    output$out_plot_sample_name_overlap <- renderPlot({
      validate(need(!is.null(maf()), message = "Dataset failed to load"))
      maftools_maf_and_gistic_sample_name_overlap_venn(maf = maf(), gistic = gistic()) 
    })
    
    

    # Analyses ----------------------------------------------------------------
    mod_plot_gistic_genome_server("mod_plot_genome", gistic=gistic, maf = maf)
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
  assertions::assert_class(gistic, "GISTIC")
  
  maf_sample_names <- maf %>% 
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  gistic_sample_names <- gistic %>%
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) 
  
  ggvenn::ggvenn(list( "SNV" = maf_sample_names, "CNV" = gistic_sample_names)) +
    ggplot2::ggtitle("Tumor Sample Barcode Overlap") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"))
}

maftools_maf_and_gistic_random_sample_names <- function(maf, gistic) {
  assertions::assert_class(gistic, "GISTIC")
  
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
