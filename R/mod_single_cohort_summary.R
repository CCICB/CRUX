
mod_single_cohort_summary_tables_ui <- function(id, panel_heading = "Summary of Input" ){
  ns <- NS(id)
  
  tagList(
    shinyWidgets::panel(heading = panel_heading,
          tabsetPanel(type = "tabs", id = ns("tabset_active_panel"),
                tabPanel(title = "All data", value = "all_data", mod_tabulate_maf_ui(id = ns("mod_tabulate_maf_all"))),
                tabPanel(title = "Summary", value = "summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_summary"))),
                tabPanel(title = "Gene level summary", value = "gene_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_gene_summary"))),
                tabPanel(title = "Sample level summary", value = "sample_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_sample_summary"))),
                tabPanel(title = "Clinical data summary", value = "clinical_data_summary", mod_render_downloadabledataframe_ui(id = ns("out_dt_maf_clinical_data_summary"))),
                br())
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
      
      #Problm
      mod_tabulate_maf_server(id = "mod_tabulate_maf_all", maf = maf)
      mod_render_downloadabledataframe_server(id = "out_dt_maf_summary", tabular_data_object = summary_df, basename = "download", rownames = F, colnames = T) 
      mod_render_downloadabledataframe_server(id = "out_dt_maf_gene_summary", tabular_data_object = gene_summary_df, basename = "download", rownames = F, colnames = T) 
      mod_render_downloadabledataframe_server(id = "out_dt_maf_sample_summary", tabular_data_object = sample_summary_df, basename = "download", rownames = F, colnames = T)
      mod_render_downloadabledataframe_server(id = "out_dt_maf_clinical_data_summary", tabular_data_object = clinical_data_df, basename = "download", rownames = F, colnames = T) 
      
      active_summary_table <- reactive({ message(input$tabset_active_panel); input$tabset_active_panel })
      
      observe({message("active_summary_table: ", active_summary_table())})
    }
  )
}


mod_single_cohort_summary_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinyWidgets::panel(heading = "Pan Cohort Analysis",
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Summary",mod_plot_maf_summary_ui(id = ns("mod_plot_summary"))),
                  tabPanel(title = "TiTv", mod_plot_titv_graphs_ui(id = ns("mod_plot_titv"))),
                  tabPanel(title = "TMB", mod_plot_tmb_in_context_of_tcga_ui(id = ns("id_plot_tcga"))),
                  tabPanel(title = "Oncoplot", mod_plot_oncoplot_ui(id = ns("mod_oncoplot"))),
                  tabPanel(title = "Mutated Pathways", mod_plot_oncogenic_pathways_ui(id = ns("mod_oncopathways"))),
                  tabPanel(title = "Pathway Specific Plots", mod_plot_oncogenic_pathways_focused_ui(id = ns("mod_oncopathways_specific"))),
                  tabPanel(title = "Lollipop", mod_plot_lollipop_ui(id = ns("mod_plot_lollipop")))
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
      mod_plot_oncogenic_pathways_server(id ="mod_oncopathways", maf = maf)
      mod_plot_oncogenic_pathways_focused_server(id = "mod_oncopathways_specific", maf = maf)
      mod_plot_lollipop_server(id = "mod_plot_lollipop", maf = maf, name_cohort = cohortName)
  }
  )
}


#' wrapper for running multiple mod_single_cohort_summary_tables_ui modules
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
mod_single_cohort_summary_tables_and_plots_ui <- function(id, print_instructions=FALSE){
  ns <- NS(id)
  uielement=ifelse(print_instructions, yes=tagList(shinyWidgets::panel(heading = "Instructions",
                                                 tags$ol(
                                                   tags$li("Import a dataset using the", tags$b( "Import MAF " ), "sidebar panel")
                                                   #tags$li("Import a clinical feature file using the ", tags$b(" Import clinical feature file"), ". See FAQ / ", tags$b("Utilies => Add clinical data")),
                                                 ),
                                                 p(
                                                   "If your data is split over several MAFs, see", tags$b(" Utilites => Merge Mafs"),".", 
                                                   "For other queries, see", tags$b(" Help "), "."
                                                 ))),
                   no=tagList())
  
  tagList(
    uielement,
    mod_single_cohort_summary_tables_ui(id = ns("maf1")),
    br(), br(),
    
    mod_single_cohort_summary_plots_ui(id=ns("maf1_plots")),
    br(), br()
    )
  }


#' wrapper for running multiple mod_single_cohort_summary_tables_server modules
#'
#' @param id 
#' @param maf maf object (reactive)
#' @param cohortName cohort Name (string, reactive)
#'
#' @return
#' @export
#'
#' @examples
mod_single_cohort_summary_tables_and_plots_server <- function(id, maf, cohortName){
  
  moduleServer(id,
    function(input, output, session){
      mod_single_cohort_summary_tables_server(id = "maf1", maf = maf, cohortName = cohortName)
      mod_single_cohort_summary_plots_server(id = "maf1_plots",  maf = maf, cohortName = cohortName)
      #mod_single_cohort_summary_tables_server(id = "maf2",= list_maf2)
      #mod_single_cohort_summary_plots_server(id = "maf2_plots",= list_maf2)
  }
  )
}

# Copy in UI
#moduleGetBasicStatsTablesui("moduleGetBasicStatsTablesui")

# Copy in server
#callModule(moduleGetBasicStatsTables, "moduleGetBasicStatsTablesui")