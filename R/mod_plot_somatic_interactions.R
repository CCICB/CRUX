#' somatic_interactions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_somatic_interactions_ui <- function(id){
  ns <- NS(id)
  tagList(
      plotOutput(outputId = ns("out_plot_somatic_interactions"), height = "650px")  %>% shinycssloaders::withSpinner(proxy.height = "200px"),
      
      br(),
    conditionalPanel(
      ns = ns,
      condition = "input.in_check_show_table",
      br(),
      DT::dataTableOutput(outputId = ns("out_dt_tabular_coocurrance_matrix")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
      br()
    ),
    
    shinyWidgets::panel(
      heading = "Options",
      fluidRow(
      shinyWidgets::prettyCheckbox(ns("in_check_show_table"), label = "Toggle Tabular Output", value = FALSE) %>% col_2(style = "margin-top: 26px"),
      numericInput(inputId = ns("in_num_topn"), value = 25, label = "Top N Genes", step = 1, min = 1)  %>% col_2(),
      shinyBS::bsTooltip(id = ns("in_num_topn"), title = "check for interactions among top 'n' number of genes. Defaults to top 25 genes"),
      shinyWidgets::pickerInput(inputId = ns("in_pick_genes"), label = "Genes", choices = c(), multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)) %>% col_2(),
      shinyBS::bsTooltip(id = ns("in_pick_genes"), title = "List of genes among which interactions should be tested. If not provided, test will be performed between top 'N' genes."),
      numericInput(inputId = ns("in_num_fontsize"), value = 0.8, label = "Font Size", step = 0.2, min = 0) %>% col_2(), 
      ),
      fluidRow(
      shinyWidgets::prettyCheckbox(ns("in_check_showsigsymbols"), label = "Toggle Significance Symbols", value = TRUE) %>% col_2(style = "margin-top: 26px"),
      numericInput(inputId = ns("in_num_size_sig_symbols"), value = 2, label = "Size of Significance Symbols", step = 0.2, min = 0) %>% col_2(),
      shinyWidgets::prettyCheckbox(ns("in_check_showcounts"), label = "Toggle Counts", value = FALSE) %>% col_2(style = "margin-top: 26px"),
      shinyWidgets::pickerInput(ns("in_pick_countstats"), label = "Counted Stat", choices = c("all", "sig")) %>% col_2(),
      shinyWidgets::pickerInput(ns("in_pick_counttype"), label = "Count Type", choices = c("all", 'cooccur', "mutexcl")) %>% col_2()
      ),
      
      
      
      moduleDownloadPlotUI(id = ns("mod_download_plot_interactions"))
    )
  )
}

#' somatic_interactions Server Functions
#'
#' @param maf a maf object (maf)
#'
mod_plot_somatic_interactions_server <- function(id, maf){
  utilitybeltshiny::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    plot_somatic_interactions = reactive({
      validate(need(!is.null(maf()), message = "Waiting for dataset to load"))
      
      function(){
        maftools::somaticInteractions(
          maf=maf(), 
          top = input$in_num_topn,
          genes = input$in_pick_genes,
          fontSize = input$in_num_fontsize,
          showSigSymbols = input$in_check_showsigsymbols,
          showCounts = input$in_check_showcounts,
          countStats = input$in_pick_countstats, 
          countType = input$in_pick_counttype, 
          sigSymbolsSize = input$in_num_size_sig_symbols
        ) %>% return()
      }
    })
    
    coocurrance_df <- reactive({ plot_somatic_interactions()() })
    output$out_dt_tabular_coocurrance_matrix <- DT::renderDataTable({coocurrance_df()}, options = list(scrollX = TRUE), class = "display nowrap")
    
    
    output$out_plot_somatic_interactions <- renderPlot({ plot_somatic_interactions()() })
    
    
    genes <- reactive({
      validate(need(!is.null(maf()), message = "Waiting for dataset to load"))
      maf() %>% maftools::getGeneSummary() %>% dplyr::pull(Hugo_Symbol) %>% sort %>% unique %>% return()
    })
    
    observeEvent(genes(), {
      isolate({
        shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_genes", choices = genes())
      })
    })
    
    
    
    moduleDownloadPlotServer(id = "mod_download_plot_interactions", session_parent = session, plotOutputId = "out_plot_somatic_interactions", plotting_function = plot_somatic_interactions(), default_filename = "somatic_interactions")
    
  })
}

## To be copied in the UI
# mod_plot_somatic_interactions_ui("somatic_interactions_ui_1")

## To be copied in the server
# mod_plot_somatic_interactions_server("somatic_interactions_ui_1")
