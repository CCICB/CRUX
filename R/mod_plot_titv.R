# titv --------------------------------------------------------------
# Creates taglist containing the plot + options panel

mod_plot_titv_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(outputId=ns("out_plot_titv"), height = "500px") %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    br(),
    shinyWidgets::panel(heading = "Options",
                        fluidRow(
          shiny::selectInput(inputId = ns("in_select_plot_type"), label = "Plot type", choices = c("both", "bar", "box"), selected = "both", multiple = F) %>% col_3(),
          checkboxInput(inputId = ns("in_check_show_barcodes"), label = "Show barcodes", value = TRUE) %>% col_3(style="margin-top: 24px"),
          checkboxInput(inputId = ns("in_check_plot_notch"), label = "Plot notch", value = FALSE) %>% col_3(style="margin-top: 24px"),
          checkboxInput(inputId = ns("in_check_use_syn"), label = "Include Synonymous Variants", value = FALSE) %>% col_3(style="margin-top: 24px"),
                        ),
          fluidRow(
            numericInput(inputId = ns("in_num_fontsize"), label = "Fontsize: Y axis titles", value = 1, min = 0, step = 0.2) %>% col_3(),
            numericInput(inputId = ns("in_num_fontsize_axis_x"), label = "Fontsize: X axis tick labels", value = 1, min = 0, step = 0.2) %>% col_3(),
            numericInput(inputId = ns("in_num_fontsize_axis_y"), label = "Fontsize: Y axis tick labels", value = 1, min = 0, step = 0.2) %>% col_3(),
            conditionalPanel(condition = "input.in_check_show_barcodes", ns=ns, numericInput(inputId = ns("in_num_fontsize_samples"), label = "Fontsize: sample names", value = 1, min = 0, step = 0.2)) %>% col_3(),
          ),
          moduleDownloadPlotUI(ns("mod_download"))
    )
  )
}

mod_plot_titv_graphs_server <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
      plot_titv <- reactive({ function() { 
        maftools::plotTiTv(maftools::titv(maf = maf(), useSyn = input$in_check_use_syn), plotType = input$in_select_plot_type, showBarcodes = input$in_check_show_barcodes, plotNotch = input$in_check_plot_notch, baseFontSize = input$in_num_fontsize, textSize = input$in_num_fontsize_samples, axisTextSize = c(input$in_num_fontsize_axis_x,input$in_num_fontsize_axis_y)) 
        } 
        })
      output$out_plot_titv <- renderPlot({ plot_titv()()})
      moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_titv", plotting_function = plot_titv(), default_filename = "titv")
  }
  )
}
