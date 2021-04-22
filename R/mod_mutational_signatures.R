refgenome_options = c("BSgenome.Hsapiens.UCSC.hg19")

#' mutational_signatures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mutational_signatures_ui <- function(id){ #THIS IS THE ONLY MODULE THAT USES BSGENOME
  ns <- NS(id)
  tagList(

    # Step 1: Select Dataset --------------------------------------------------
    mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_ui(id = ns("mod_select_dataset_wrapper"), panel = TRUE),
    icon_down_arrow(),br(),
    
    
    
    # Step 2: Select Reference Genome --------------------------------------------------
    shinyWidgets::panel(
      heading = "Select Reference Genome",
      shinyWidgets::pickerInput(ns("in_pick_genome"),label = "Reference Genome", choices = c(hg19 = "BSgenome.Hsapiens.UCSC.hg19", hg38 = "BSgenome.Hsapiens.UCSC.hg38"))
    ),
    
    icon_down_arrow(), br(),
    
    # Step 3: Make sure reference genome is downloaded --------------------------------------------------
    shinyWidgets::panel(
      heading = "Is reference genome installed?",
      
      conditionalPanel(
        ns=ns, 
        condition = "output.ref_is_downloaded == false",
        html_alert(text = "Selected reference genome is not installed. Please hit the button below to download it (You will only have to do this once)", status = "danger"),
        actionButton(ns("in_bttn_download_genome"), label = "Download reference genome"),
        textOutput(ns("out_text_install_log"))
      ),
      
      conditionalPanel(
        ns=ns, 
        condition = "output.ref_is_downloaded == true",
        html_alert(text = "Reference genome is installed and ready to go", status = "success")
      )
      
    ),
    conditionalPanel(ns=ns, condition = "output.ref_is_downloaded == true",
      icon_down_arrow(), br(),
      # Step 4: Look at TNM matrix ----------------------------------------------
      shinyWidgets::panel(heading = "Results",
      tabsetPanel(
        tabPanel(title = "Trinucleotide Matrix", mod_render_downloadabledataframe_ui(ns("mod_text_matrix"),shinycssloader = TRUE )),
        tabPanel(title = "APOBEC Differences", mod_plot_apobec_diff_ui(ns("mod_plot_apobec"))),
        tabPanel(title = "Signature Analysis",
                 plotOutput(ns("out_plot_cophenetic"))
                 )
        )
    )
    )
    
    
    
    # Step 5: Differences between APOBEC enriched and non-enriched samples  ----------------------------------------------
    
    
    # Step 6: ----------------------------------------------
    
    
    #Debug
    
    
  )
}
    
#' mutational_signatures Server Functions
#'
#' @noRd 
mod_mutational_signatures_server <- function(id, maf_data_pool){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    maf_data_wrapper <- mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_dataset_wrapper_server("mod_select_dataset_wrapper", maf_data_pool = maf_data_pool, label = "Step 1: Select Dataset")
    maf <- reactive({maf_data_wrapper()$loaded_data})
    reset <- reactiveVal()
    
    # Step 2: Select Reference Genome --------------------------------------------------
    
    # Step 3: Make sure reference genome is downloaded --------------------------------------------------
    output$ref_is_downloaded <- reactive({
      reset()
      input$in_pick_genome %in% BSgenome::installed.genomes()
      })
    outputOptions(output, "ref_is_downloaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$in_bttn_download_genome, isolate({
      updateActionButton(session = session, inputId = "in_bttn_download_genome", label = "Installing package ...")
      shinyjs::disable("in_bttn_download_genome")
      
      shinyWidgets::sendSweetAlert(session = session, title = "Download starting", text = "Reference genome download has commenced. While the genome is downloading no other shinymaftools functionality will work", showCloseButton = FALSE, closeOnClickOutside = FALSE, type = "info")
      BiocManager::install(input$in_pick_genome, update = FALSE, ask = FALSE)
      shinyWidgets::sendSweetAlert(session = session, title = "Download Complete", text = "Reference genome has been installed", showCloseButton = FALSE, closeOnClickOutside = FALSE, type = "success")
      reset(input$in_pick_genome)
      #shinyjs::reset(id = "ref_is_downloaded")
      shinyjs::enable("in_bttn_download_genome") 
      }))
    
    # Step 4: Run Analysis and visualie results --------------------------------------------------
    
    trinucleotide_matrix <- reactive({
      #browser()
      maftools::trinucleotideMatrix(maf = maf(), prefix = 'chr', add = TRUE, ref_genome = input$in_pick_genome, useSyn = TRUE)
    })
    
    trinucleotide_matrix_df <- reactive({
      #browser()
        trinucleotide_matrix()$nmf_matrix %>% as.data.frame()
      })
    
    # Visualise Results ----------------------------------------------
    #Trinucleotide matrix
    mod_render_downloadabledataframe_server("mod_text_matrix", tabular_data_object = trinucleotide_matrix_df, basename = "trinucleotide_matrix", filter = "none")
    
    #Plot apobec differences
    mod_plot_apobec_diff_server("mod_plot_apobec", maf = maf, tnm = trinucleotide_matrix)
    

    # Step 5: Signature analysis ----------------------------------------------
    signatures <- reactive({
      #library(NMF)
      #Make sure user is sure: implement later
      shinyWidgets::confirmSweetAlert(session = session, inputId = "in_confirm_sweet_alert", text = "Signature analysis can take a long time, are you sure you want to continue", closeOnClickOutside = FALSE, showCloseButton = FALSE)
      #maftools::estimateSignatures(mat = trinucleotide_matrix(), nTry = 6, parallel = 4 ) # Add NMF:: calls on functions
    })
    
    output$out_plot_cophenetic <- renderPlot({
       # maftools::plotCophenetic(signatures(), bestFit = 1)
      })
    
    
  })
}
    
## To be copied in the UI
# mod_mutational_signatures_ui("mutational_signatures_ui_1")
    
## To be copied in the server
# mod_mutational_signatures_server("mutational_signatures_ui_1")
