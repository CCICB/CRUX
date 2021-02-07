# oncogenic_pathways --------------------------------------------------------------
# Creates taglist containing the table + plot + options panel

mod_plot_oncogenic_pathways_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_render_downloadabledataframe_ui(id = ns("mod_downloadable_table_oncogenic_pathways")),
    hr(),
    plotOutput(outputId=ns("out_plot_oncogenic_pathways")) %>% shinycssloaders::withSpinner(), 
    shinyWidgets::panel(heading = "Options",
        #shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_use_custom_genes"), label = "Custom genes", value = FALSE),
        #conditionalPanel(condition = "input.in_checkbox_use_custom_genes", ns=ns, uiOutput(outputId = ns("out_ui_genelist"
        moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

mod_plot_oncogenic_pathways_server <- function(id, maf){
  utilitybeltshiny::assert_reactive(maf)
  
  moduleServer(id,
    function(input, output, session){
      
      #Validate
      maf_validated <- reactive({ validate(need(!is.null(maf()),message = "Loading ..." )); return(maf()) })
      
      #Plot
      plot_oncogenic_pathways <- reactive({ 
        validate(need(!is.null(maf_validated()),message = "Loading ... ")); 
        function() { maftools::OncogenicPathways(maf = maf_validated()) } })
      
      output$out_plot_oncogenic_pathways <- renderPlot({plot_oncogenic_pathways()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_oncogenic_pathways", plotting_function = plot_oncogenic_pathways(), default_filename = "oncogenic_pathways")
      
      #Enrichent Dataframe
      enrichment_table <- reactive({ plot_oncogenic_pathways()() %>% return()})
      mod_render_downloadabledataframe_server(id = "mod_downloadable_table_oncogenic_pathways", tabular_data_object = enrichment_table, basename = "pathway_enrichment", rownames = F, colnames = T)
  }
  )
}
