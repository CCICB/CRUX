#' plot_rainfall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_rainfall_ui <- function(id){
  ns <- NS(id)
  tagList(
      
      plotOutput(ns("out_plot_rainfall")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
      br(),hr(),
      tags$strong("Kataegis detected at:"),
      mod_render_downloadabledataframe_ui(id = ns("out_dt_kataegis"), downloadbttn_label = "", shinycssloader = FALSE),
      #DT::dataTableOutput(ns("out_dt_kataegis")),
      br(),
      shinyWidgets::panel(
        heading="Options",
        
        fluidRow(
          col_3(
        shinyWidgets::awesomeRadio(
            inputId = ns("in_radio_ref"),
            label = "Reference Genome", 
            choices = c("hg18", "hg19", "hg38"),
            selected = "hg19",
            inline = TRUE, 
            checkbox = TRUE
          )
        ),
        shinyBS::bsTooltip(id = ns("in_radio_ref"), "Select the reference genome your variant data was called against  (THIS DOES NOT DO A LIFTOVER). If Unsure, don't stress. This setting changes chromosome spacing in plot but NOT the kataegis coordinates identified as those are identified independent of chromosome length"),
        
        col_3(
        numericInput(
          ns("in_num_pointsize"),
          label = "Point Size", 
          value = 0.4, 
          min=0.01, 
          step = 0.05  
          )
        ),
        col_3(
        numericInput(
          ns("in_num_fontsize"),
          label = "Font Size", 
          value = 1.2, 
          min=0.01, 
          step = 0.2  
        )
        ),
        
        col_3(
          moduleDownloadPlotUI(id = ns("mod_download_plot"), style = "margin-top: 25px; height: 34px")
        )
        )
      )
  )
}

#' plot_rainfall Server Functions
#'
#' @noRd 
mod_plot_rainfall_server <- function(id, maf, tsb){
  utilitybeltshiny::assert_reactive(maf)
  utilitybeltshiny::assert_reactive(tsb)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_validated <- reactive({ validate(need(!is.null(maf()),message = "Please select a dataset" )); return(maf()) })
    tsb_validated <- reactive({ validate(need(!is.null(tsb()),message = "Please select a dataset" )); return(tsb()) })
    refbuild <- reactive({ validate(need(!is.null(input$in_radio_ref), message = "Loading reference genome chromosome sizes" )); return(input$in_radio_ref) })
    
    # Plotting Function -------------------------------------------------------
    plot_rainfall <- reactive({
      validate(need(!is.null(input$in_radio_ref), message = " Loading selected reference..."))
      #browser()
      function(){
        maftools_plot_rainfall(
          maf=maf_validated(), 
          tsb = tsb_validated(), 
          detectChangePoints = TRUE, 
          pointSize = input$in_num_pointsize, 
          fontSize = input$in_num_fontsize,
          ref.build = "hg19"
          )
        #maftools::rainfallPlot(maf = maf_validated(), tsb = tsb_validated(), detectChangePoints = TRUE, savePlot = FALSE, ref.build = "hg19")
      }
    })
    
    
    # Render Plot -------------------------------------------------------------
    output$out_plot_rainfall <- renderPlot({
      plot_rainfall()()
    })


    
    # Render Datatable --------------------------------------------------------
    predicted_kataegis_df <- reactive({
      predicted_kataegis_df_ <- plot_rainfall()()
    })
    
    predicted_kataegis_df_validated <- reactive({ validate(need(!is.null(predicted_kataegis_df()),message = " No kataegis predicted" )); return(predicted_kataegis_df()) })
    
    mod_render_downloadabledataframe_server(id = "out_dt_kataegis", tabular_data_object = predicted_kataegis_df_validated, basename = filename, colnames = TRUE)
    
    filename <- reactive({
      paste0(tsb())
      })

    # Download Plot -----------------------------------------------------------
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_rainfall", plotting_function = plot_rainfall(), default_filename = filename())
  })
}

## To be copied in the UI
# mod_plot_rainfall_ui("plot_rainfall_ui_1")

## To be copied in the server
# mod_plot_rainfall_server("plot_rainfall_ui_1")
