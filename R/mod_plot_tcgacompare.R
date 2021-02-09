

# creates taglist with plot + options panel
mod_plot_tmb_in_context_of_tcga_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId = ns("out_plot_tcgacompare")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    br(),
    shinyWidgets::panel(heading = "Options",
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_known_capture_size"), label = "Known Capture Size", value = FALSE),
          conditionalPanel(condition = "input.in_checkbox_known_capture_size", ns = ns,
                           numericInput(inputId = ns("in_num_capturesize"), label = "Capture Size (MB)", value = 50),
                           numericInput(inputId = ns("in_num_tcgacapturesize"), label = "TCGA Capture Size (MB)", value = 50)),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_primary_site"), label = "Use primary site labels", value = FALSE),
          uiOutput(outputId = ns("out_ui_tcga_cohorts")),
          moduleDownloadPlotUI(id = ns("mod_download_plot"))
          )
    )
}



mod_plot_tmb_in_context_of_tcga_server <- function(id, maf, cohortName){
  moduleServer(id,
    function(input, output, session){
      utilitybeltshiny::assert_reactive(maf)
      utilitybeltshiny::assert_reactive(cohortName)
      
      tcga_cohorts <- reactive({ validate(need(expr = { 
        file.exists(system.file("extdata/files_required_for_tmb_comparison/TCGA_primary_site", package="shinymaftools")) & file.exists(system.file("extdata/files_required_for_tmb_comparison/TCGA_cohorts", package="shinymaftools")) }, 
        message = "extdata/files_required_for_tmb_comparison/TCGA_primary_site OR extdata/files_required_for_tmb_comparison/TCGA_cohorts files do not exist."))
        if(input$in_checkbox_primary_site) return(readLines(system.file("extdata/files_required_for_tmb_comparison/TCGA_primary_site", package="shinymaftools"))) else return(readLines(system.file("extdata/files_required_for_tmb_comparison/TCGA_cohorts", package="shinymaftools"))) })
      
      
      plot_tga <- reactive({ function(){ maftools::tcgaCompare(maf = maf(), cohortName = cohortName(), capture_size = capturesize(), tcga_capture_size = input$in_num_tcgacapturesize, primarySite = input$in_checkbox_primary_site, tcga_cohorts = input$in_pick_tcga_cohorts)} })
      
      output$out_ui_tcga_cohorts <- renderUI({ shinyWidgets::pickerInput( inputId = session$ns("in_pick_tcga_cohorts"), label = "tcga_cohorts", choices = tcga_cohorts() %>% sort, options = shinyWidgets::pickerOptions( liveSearch = TRUE, actionsBox = TRUE), multiple = TRUE) })
      capturesize <- reactive({ if(input$in_checkbox_known_capture_size == T) return(input$in_num_capturesize) else return(NULL) })
      output$out_plot_tcgacompare <- renderPlot({ plot_tga()() })
      
      moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_tcgacompare", plotting_function = plot_tga(), default_filename = "tcga_cohort")
  }
  )
}

