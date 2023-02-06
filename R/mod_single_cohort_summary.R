
mod_single_cohort_summary_tables_ui <- function(id, panel_heading = "Tabular Summary" ){
  ns <- NS(id)
  
  tagList(
    shinyWidgets::panel(heading = panel_heading,
          tabsetPanel(type = "tabs", id = ns("tabset_active_panel"),
            tabPanel(title = "Summary", value = "summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_summary"))),
            tabPanel(title = "Gene level summary", value = "gene_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_gene_summary"))),
            tabPanel(title = "Sample level summary", value = "sample_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_sample_summary"))),
            tabPanel(title = "Clinical data summary", value = "clinical_data_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_clinical_data_summary"))),
            tabPanel(title = "All data", value = "all_data", mod_tabulate_maf_ui(id = ns("mod_tabulate_maf_all"))),
          ),
          br()
  ))
}


mod_single_cohort_summary_tables_server <- function(id, maf, cohortName){
  moduleServer(id,
    function(input, output, session){
      output$maf_is_present <- reactive({ !is.null(maf()) })
      outputOptions(output, "maf_is_present", suspendWhenHidden = FALSE)
      
      
      summary_df <- reactive({ validate(need(!is.null(maf()), "Please import dataset")); maf()@summary })
      gene_summary_df <- reactive({validate(need(!is.null(maf()), "Please import dataset")); maftools::mafSummary(maf())$gene.summary })
      sample_summary_df <- reactive({ validate(need(!is.null(maf()), "Please import dataset"));maftools::getSampleSummary(maf()) })
      clinical_data_df <- reactive({  validate(need(!is.null(maf()), "Please import dataset")); maftools::getClinicalData((maf())) })
      
      
      mod_tabulate_maf_server(id = "mod_tabulate_maf_all", maf = maf)
      mod_render_downloadabledataframe_server(id = "out_dt_maf_summary", tabular_data_object = summary_df, basename = "download", rownames = F, colnames = T) 
      mod_render_downloadabledataframe_server(id = "out_dt_maf_gene_summary", tabular_data_object = gene_summary_df, basename = "download", rownames = F, colnames = T) 
      mod_render_downloadabledataframe_server(id = "out_dt_maf_sample_summary", tabular_data_object = sample_summary_df, basename = "download", rownames = F, colnames = T)
      mod_render_downloadabledataframe_server(id = "out_dt_maf_clinical_data_summary", tabular_data_object = clinical_data_df, basename = "download", rownames = F, colnames = T) 
      
      active_summary_table <- reactive({ message(input$tabset_active_panel); input$tabset_active_panel })
    
    }
  )
}


mod_single_cohort_summary_plots_ui <- function(id, heading = "Visualisations"){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::panel(heading = heading,
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Summary",mod_plot_maf_summary_ui(id = ns("mod_plot_summary"))),
                  tabPanel(title = "Oncoplot", mod_plot_oncoplot_ui(id = ns("mod_oncoplot"))),
                  tabPanel(title = "TMB", mod_plot_tmb_in_context_of_tcga_ui(id = ns("id_plot_tcga"))),
                  tabPanel(title = "TiTv", mod_plot_titv_graphs_ui(id = ns("mod_plot_titv"))),
                  tabPanel(title = "Drug-Gene Interactions", mod_druggability_ui(id = ns("mod_druggability"))),
                  #tabPanel(title = "Mutated Pathways", mod_plot_oncogenic_pathways_ui(id = ns("mod_oncopathways"))),
                  tabPanel(title = "Pathway Specific Plots", mod_plot_oncogenic_pathways_focused_ui(id = ns("mod_oncopathways_specific"))),
                  tabPanel(title = "Lollipop", mod_plot_lollipop_ui(id = ns("mod_plot_lollipop"))),
                  tabPanel(title = "Somatic Interactions", mod_plot_somatic_interactions_ui(id = ns("mod_somatic_interactions"))),
                  tabPanel(title = "Pfam Domains", mod_plot_pfam_domains_ui(id = ns("mod_pfam_domains")))
                  ), 
      br()
    )
    )
  }

mod_single_cohort_summary_plots_server <- function(id, maf, cohortName) {
  moduleServer(id,
    function(input, output, session){
      mod_plot_maf_summary_server(id = "mod_plot_summary", maf = maf)
      mod_plot_titv_graphs_server(id = "mod_plot_titv", maf = maf)
      mod_plot_tmb_in_context_of_tcga_server(id = "id_plot_tcga", maf = maf, cohortName = cohortName)
      mod_plot_oncoplot_server(id="mod_oncoplot", maf)
      #mod_plot_oncogenic_pathways_server(id ="mod_oncopathways", maf = maf)
      mod_plot_oncogenic_pathways_focused_server(id = "mod_oncopathways_specific", maf = maf)
      mod_plot_lollipop_server(id = "mod_plot_lollipop", maf = maf, name_cohort = cohortName)
      mod_plot_somatic_interactions_server(id = "mod_somatic_interactions", maf = maf)
      mod_plot_pfam_domains_server(id = "mod_pfam_domains", maf=maf)
      mod_druggability_server(id = "mod_druggability", maf)
      
  }
  )
}


#' wrapper for running multiple mod_single_cohort_summary_tables_ui modules
#'
#' @param id 
#'
#'
#'
mod_single_cohort_summary_tables_and_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_single_cohort_summary_plots_ui(id=ns("mod_cohort_summary_plots"), heading = "Step 2: Explore Visualisations"),
    br(),
    mod_single_cohort_summary_tables_ui(id = ns("mod_cohort_summary_tables"))
    
    )
  }


#' wrapper for running multiple mod_single_cohort_summary_tables_server modules
#'
#' @param id 
#' @param maf maf object (reactive)
#' @param cohortName cohort Name (string, reactive)
#'
#'
#'
mod_single_cohort_summary_tables_and_plots_server <- function(id, maf, cohortName){
  
  moduleServer(id,
    function(input, output, session){
      mod_single_cohort_summary_tables_server(id = "mod_cohort_summary_tables", maf = maf, cohortName = cohortName)
      mod_single_cohort_summary_plots_server(id = "mod_cohort_summary_plots",  maf = maf, cohortName = cohortName)
  }
  )
}

# Copy in UI
#moduleGetBasicStatsTablesui("moduleGetBasicStatsTablesui")

# Copy in server
#callModule(moduleGetBasicStatsTables, "moduleGetBasicStatsTablesui")
