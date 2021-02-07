#' plot_pfam_domains UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_pfam_domains_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    wellPanel("What domains in a given cancer cohort are most frequently affected?"),
    
    plotOutput(outputId = ns("out_plot_pfam")) %>% shinycssloaders::withSpinner(),
    br(),
    conditionalPanel(
      condition = "input.in_check_toggle_domain_summary",
      ns = ns,
      hr(),
      DT::dataTableOutput(outputId = ns("out_dt_pfam_domain_summary")),
      br()
    ),
    #shinyWidgets::panel(heading = "Options",
    wellPanel(
                        htmlOutput(outputId = ns("out_html_explain_how_to_pick_aacol")),
                        shinyWidgets::prettyCheckbox(inputId = ns("in_check_toggle_domain_summary"), label = "Toggle Domain Summary", value = FALSE),
                        conditionalPanel(
                          condition = "!output.maf_colnames_autosearch_succeeded", ns=ns,
                          shinyWidgets::pickerInput(inputId = ns("in_pick_aacol"), label = "AACol", choices = c()),
                          shinyBS::bsTooltip(id = ns("in_pick_aacol"), title = "The column listing amino acid changes"),
                        ),
                        shinyWidgets::pickerInput(inputId = ns("in_pick_varclass"), label = "Variant Class", choices = c("nonSyn", "Syn", "all") %>% magrittr::set_names(c("Non Synonymous", "Synonymous", "All"))),
                        shinyWidgets::pickerInput(inputId = ns("in_pick_summarizeby"), label = "Summarize By", choices = c("AAPos", "AAChange")),
                        shinyBS::bsTooltip(id = ns("in_pick_summarizeby"), title = "Summarize domains by amino acid position or conversions"),
                        
                        numericInput(inputId = ns("in_num_topn"), label = "Label Top N", value = 5,min = 0, step = 1),
                        numericInput(inputId = ns("in_num_labelsize"), label = "Label Size", value = 1,min = 0, step = 0.2),
                        moduleDownloadPlotUI(id = ns("mod_download"))
                        
    )
    
  )
}

#' plot_pfam_domains Server Functions
#'
#' @noRd 
mod_plot_pfam_domains_server <- function(id, maf){
  utilitybeltshiny::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_colnames <- reactive({
      #browser()
      validate(need(!is.null(maf()), message = "Waiting for dataset to load")) 
      maf()@data %>% colnames() %>% sort %>% unique
    })
    
    maf_colnames_contains_aachange <- reactive({any(c('HGVSp_Short','AAChange', "Protein_Change") %in% maf_colnames())})
    output$maf_colnames_autosearch_succeeded = reactive({maf_colnames_contains_aachange()})
    outputOptions(output, "maf_colnames_autosearch_succeeded", suspendWhenHidden = FALSE)  
    
    output$out_html_explain_how_to_pick_aacol <- renderText({
      #browser()
      if(maf_colnames_contains_aachange()) 
        return(p("The column containing protein changes has been automatically found", class="text-success") %>% as.character())
      else
        return(p("You need to identify the column in your dataset that lists amino acid changes. Entries will look something like: p.G288D, p.R711Q, etc.")  %>% as.character())
    })
    
    
    observeEvent(maf_colnames(), { 
      isolate({ 
        #browser()
        shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_aacol", choices = maf_colnames(), selected = ifelse(maf_colnames_contains_aachange(), "AAChange", character(0)) )
      })
    })
    
    plot_pfam <- reactive({
      validate(need(!is.null(maf()), message = "Waiting for dataset to load"))
      if (!maf_colnames_contains_aachange()) 
        validate(need(!is.null(input$in_pick_aacol) & length(input$in_pick_aacol) == 1, message = "Please indicate which colum contains protein variation data AACol"))
      function(){
        maftools::pfamDomains(
          maf = maf(),
          AACol = input$in_pick_aacol, 
          summarizeBy = input$in_pick_summarizeby, 
          top = input$in_num_topn, 
          labelSize = input$in_num_labelsize,
          varClass = input$in_pick_varclass
        ) 
      }
    })
  
  pfam_domain_summary_df <- reactive({
    pfam_res_ls <- plot_pfam()()
    #browser()
    pfam_res_ls$domainSummary
  })
  
  output$out_dt_pfam_domain_summary <- DT::renderDataTable({pfam_domain_summary_df()}, options = list(scrollX = TRUE), class = "display nowrap")
  
  output$out_plot_pfam <- renderPlot({ 
    plot_pfam()()
  })
  
  moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_pfam", plotting_function = plot_pfam(), default_filename = "pfam")
  })
}

## To be copied in the UI
# mod_plot_pfam_domains_ui("plot_pfam_domains_ui_1")

## To be copied in the server
# mod_plot_pfam_domains_server("plot_pfam_domains_ui_1")
