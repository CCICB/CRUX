#' plot_druginteractions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_druginteractions_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
    # Step  1 Plot Druggability of cohort ---------------------------------------
    
    shinyWidgets::panel(
      heading = "Step 1: Configure Analysis",
      shinyWidgets::radioGroupButtons(inputId = ns("in_radiogroup_geneset"), label = "Genes To Analyse", choices = c("Mutated in many samples" = "TopN", "Manually Selected"= "Manual")),
      
      conditionalPanel(
        ns=ns, 
        condition = "input.in_radiogroup_geneset == 'TopN'",
        numericInput(inputId = ns("in_num_topn_genes"), label = "Analyse top [N] genes", value = 20, min = 1, step = 1),                 
      ),
      conditionalPanel(
        ns=ns, 
        condition = "input.in_radiogroup_geneset == 'Manual'",
        shinyWidgets::pickerInput(ns("in_pick_genes"), label = "Genes", choices = NULL, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
      ),
    ),
    
    icon_down_arrow(),br(),
    #hr(),
    
    shinyWidgets::panel(
      heading = "Step 2: Examine Druggable Categories",
      plotOutput(ns("out_plot"), height = "650px") %>% shinycssloaders::withSpinner(),
      hr(),
      shinyWidgets::panel(
        heading = "Options",
        shinyWidgets::radioGroupButtons(inputId = ns("in_radiogroup_plot_type"), label = "Plot Type", choices = c("bar", "pie"), selected = "bar"),
        numericInput(inputId = ns("in_num_fontsize"), label = "Font size", value = 0.8, min = 0.01, step = 0.2),
        moduleDownloadPlotUI(id = ns("mod_download_plot"))
      ),
    ),
    icon_down_arrow(),br(),
    
    #hr(),
    shinyWidgets::panel(
      heading = "Step 3: Examine Drug-Gene Interaction Table",
      mod_render_downloadabledataframe_ui(ns("mod_downloadable_dataframe"))
    ),
    
    br()
    
    
  )
  
}

#' plot_druginteractions Server Functions
#'
#' @noRd 
mod_plot_druginteractions_server <- function(id, maf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Populate Genelist -------------------------------------------------------
    genelist <- reactive({
      validate(need(!is.null(maf()), message = "Waiting for dataset to load ..."))
      maf() %>% 
        maftools::getGeneSummary() %>%
        dplyr::pull(Hugo_Symbol) %>%
        sort() %>%
        unique() 
    })
    
    observeEvent(genelist(), isolate({
      shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_genes", choices = genelist())
    }))
    
    # Construct Plotting Function ---------------------------------------------
    plotting_function <- reactive({
      
      function(){
        if(input$in_radiogroup_geneset == "TopN") 
          chosen_genes = NULL
        else 
          chosen_genes = input$in_pick_genes
        maftools::drugInteractions(maf = maf(), top = input$in_num_topn_genes, plotType = input$in_radiogroup_plot_type, fontSize = input$in_num_fontsize, genes = chosen_genes,  drugs = FALSE)
      }
    })
    
    
    # Render Table ------------------------------------------------------------
    interactions_df <- reactive({ 
      tryCatch(
        expr = { 
          if(input$in_radiogroup_geneset == "TopN") 
            chosen_genes = NULL
          else 
            chosen_genes = input$in_pick_genes
          
          maftools::drugInteractions(maf = maf(), top = input$in_num_topn_genes, genes = chosen_genes, drugs = TRUE) %>% type.convert(as.is=TRUE)
        },
        error = function(err){
          err_char = as.character(err)
          if (grepl("No claimed drugs found for given genes", err_char)) 
            validate("No claimed drugs found for given genes")
          else 
            validate(err_char)
        }
      )
    })
    mod_render_downloadabledataframe_server(id = "mod_downloadable_dataframe", tabular_data_object = interactions_df)
    
    
    output$out_plot <- renderPlot({ plotting_function()() })
    
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot", plotting_function = plotting_function(), default_filename = "drug_interactions")
  })
}

## To be copied in the UI
# mod_plot_druginteractions_ui("plot_druginteractions_ui_1")

## To be copied in the server
# mod_plot_druginteractions_server("plot_druginteractions_ui_1")
