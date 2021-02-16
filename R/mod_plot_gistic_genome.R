#' plot_gistic_genome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_gistic_genome_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    div(
      plotOutput(outputId = ns("out_plot_chromplot")),
      html_alert(text = "G-score is a function of the amplitude of aberrations as well as the frequency of its occurrence across samples", status = "info")
    ) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    hr(),
    shinyWidgets::panel(
      heading="Options",
      fluidRow(
        shinyWidgets::pickerInput(ns("in_pick_mutgenes"), label = "Genes to Highlight", choices = c(), multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)) %>% col_3(),
        shinyWidgets::pickerInput(ns("in_pick_cytobands"), label = "Cytobands To Label", choices = c(), multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)) %>%
          bsplus::bs_embed_tooltip(title="Which cytobands to label. Defaults to top 5 lowest q values") %>% col_3(),
        numericInput(ns("in_num_fdrcutoff"), label = "FDR cutoff", value = 0.1, min = 0, max = 1, step = 0.05) %>% col_3(),
        shinyWidgets::prettyRadioButtons(ns("in_radio_ref.build"), "Reference Genome", choices = c("hg18", "hg19", "hg38"), selected = "hg19", inline = TRUE) %>% col_3(),
      ),
      fluidRow(
        numericInput(ns("in_num_txt_size"), label = "Font Size", value = 0.8, min = 0.01, step = "0.1") %>% col_3(),
        numericInput(ns("in_num_cytoband_txt_size"), label = "Cytoband Font Size", value = 0.6, min = 0.01, step = 0.1) %>% col_3(),
        numericInput(ns("in_num_mtgenes_txt_size"), label = "Gene Font Size", value = 0.6, min = 0.01, step = 0.1) %>% col_3(),
        numericInput(ns("in_num_cytoband_offset"), label = "Cytoband Offset", value = 0.01, min = 0, step = "0.005") %>%
          bsplus::bs_embed_tooltip(title="If scores.gistic file is given use this to adjust cytoband size.") %>% col_3(),
      ),
      moduleDownloadPlotUI(id = ns("mod_download_plot"), width = "100%")
    )
  )
}

#' plot_gistic_genome Server Functions
#'
#' @noRd 
mod_plot_gistic_genome_server <- function(id, gistic, maf){
  utilitybeltshiny::assert_reactive(gistic)
  utilitybeltshiny::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #maf_validated <- reactive({ validate(need(!is.null(maf()), message = "Loading ..." )); return(maf()) })
    
    mutGenes <- reactive({
      validate(need(!is.null(maf()), message = "Loading ..."))
      genelist <- maf() %>%
        maftools::getGeneSummary() %>%
        dplyr::pull(Hugo_Symbol) %>%
        sort() %>%
        unique()
    })
    observeEvent(mutGenes(), { shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_mutgenes", choices = mutGenes(), selected = character(0))})
    
    cytobands <- reactive({
      validate(need(!is.null(gistic()), message = "Loading ..."))
      maftools::getCytobandSummary(gistic()) %>% 
        dplyr::pull(Cytoband) %>%
        sort() %>%
        unique()
    })
    
    observeEvent( cytobands() , {
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_cytobands", choices = cytobands(), selected = character(0))
    })
    
    
    plotting_function <- reactive({
      function(){
        validate(need(!is.null(gistic()), message = "Please import data"))
        maftools::gisticChromPlot(
          gistic = gistic(),
          fdrCutOff = input$in_num_fdrcutoff,
          ref.build = input$in_radio_ref.build,
          cytobandOffset = input$in_num_cytoband_offset,
          markBands = input$in_pick_cytobands, 
          cytobandTxtSize = input$in_num_cytoband_txt_size, 
          txtSize = input$in_num_txt_size,
          maf = maf(),
          mutGenes = input$in_pick_mutgenes,
          mutGenesTxtSize = input$in_num_mtgenes_txt_size,
        )
      }
    })
    
    output$out_plot_chromplot <- renderPlot({ 
      #browser()
      plotting_function()()
    })
    
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_chromplot", plotting_function = plotting_function(), default_filename = "CNV_genome_plot")
  })
}

## To be copied in the UI
# mod_plot_gistic_genome_ui("plot_gistic_genome_ui_1")

## To be copied in the server
# mod_plot_gistic_genome_server("plot_gistic_genome_ui_1")
