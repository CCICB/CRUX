# summary --------------------------------------------------------------
# Creates taglist containing the plot + options panel

mod_plot_maf_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_summary")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    
    shinyWidgets::panel(
      heading = "Options",
      fluidRow(
        col_3(
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_rm_outlier"), label = "Remove outliers from boxplot", value = TRUE),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_dashhboard"), label = "Dashboard style", value = TRUE),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_titv_raw"), label = "Plot titv raw counts instead of fractions", value = TRUE),
          shinyWidgets::awesomeCheckbox(inputId = ns("in_check_logscale"), label = "Logscale", value = FALSE),
          ),
        col_3(
          selectInput(inputId = ns("in_select_stat"), label = "Add Statistic", choices = list("none", "median", "mean"), selected = "none", multiple = F),
          numericInput(inputId = ns("in_num_top_genes"), label = "Show Top N genes", value = 10, min = 1, step = 1),
          
          ),
      col_3(
        numericInput(inputId = ns("in_num_fontsize_base"), label = "Fontsize: Base", value = 1, min = 0, step = 0.2),
        numericInput(inputId = ns("in_num_fontsize_title"), label = "Fontsize: Title", value = 1, min = 0, step = 0.2),
        ),
      col_3(
        numericInput(inputId = ns("in_num_fontsize_subtitle"), label = "Fontsize: Subtitle", value = 0.8, min = 0, step = 0.1),
        shinyWidgets::awesomeCheckbox(inputId = ns("in_check_show_barcodes"), label = "Show Sample Names", value = FALSE),
        conditionalPanel(condition = "input.in_check_show_barcodes", ns = ns, numericInput(inputId = ns("in_num_fontsize_samples"), label = "Fontsize: Samples", value = 0.8, min = 0, step = 0.2)),
        ),
      
      ),
      moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

mod_plot_maf_summary_server <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
      selected_stat <- reactive({  if(input$in_select_stat == "none") return(NULL) else return(input$in_select_stat)})
      
      output$out_plot_summary <- renderPlot({ plotmafSummary(maf = maf()) })
      plot_summary <- reactive({ function() {
        my_maf=maf()
        maftools::plotmafSummary(maf = my_maf,
                                 rmOutlier = input$in_check_rm_outlier,
                                 dashboard = input$in_check_dashhboard,
                                 log_scale = input$in_check_logscale,
                                 addStat = selected_stat(),
                                 showBarcodes = input$in_check_show_barcodes,
                                 top = input$in_num_top_genes,
                                 textSize = input$in_num_fontsize_samples,
                                 fs = input$in_num_fontsize_base,
                                 titleSize = c(input$in_num_fontsize_title, input$in_num_fontsize_subtitle),
                                 titvRaw = input$in_check_titv_raw
                                 )
        }})
      output$out_plot_summary <- renderPlot({plot_summary()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_summary", plotting_function = plot_summary(), default_filename = "summary")
  }
  )
}
