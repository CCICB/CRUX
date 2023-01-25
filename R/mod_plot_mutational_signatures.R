#' plot_mutational_signatures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_mutational_signatures_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("out_plot_stacked_bar")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    
    shinyWidgets::panel(
      heading="Options",
      fluidRow(
        shiny::selectInput(ns("in_select_lump_type"),label = "Lump by", choices = c("min_prop", "topn", "none"), multiple = FALSE, width = "100%") %>% column(width = 4),
        
        column(width = 8,
               conditionalPanel("input.in_select_lump_type == 'min_prop'", ns = ns,
                                shiny::numericInput(ns("in_num_lump_min"),label = "Combine signatures with contibutions below:", value =  0.1, min = 0, max = 1, step = 0.1, width = "100%"),
               )),
        
        column(width = 8,
               conditionalPanel("input.in_select_lump_type == 'topn'", ns = ns,
                                shiny::numericInput(ns("in_num_topn"),label = "Number of signatures to visualise (all others will be lumped together as other):", value =  5, min = 1, step = 1, width="100%"),
               ))
      ),
      
      fluidRow(
        shiny::selectInput(ns("in_select_facet_column"), label = "Facet by", choices = NA, selectize = TRUE) %>% column(width = 3),
        #shiny::numericInput(ns("in_num_facet_ncol"),label = "Number of columns", value = 1, min = 1, step = 1) %>% column(width = 3),
        shiny::numericInput(ns("in_num_fontsize_strip"),label = "Fontsize: strip titles", value = 18, min = 0.01, step = 2) %>% column(width = 3),
        shiny::numericInput(ns("in_num_fontsize_axis_titles"),label = "Fontsize: axis titles", value = 18, min = 0.01, step = 2) %>% column(width = 3)
      ),
      
      shiny::selectInput(ns("in_select_legend"),label = "Legend Position", choices = c("right",  "left", "top", "bottom"), multiple = FALSE),
      moduleDownloadPlotUI(id = ns("mod_download"))
    )
  )
}

#' plot_mutational_signatures Server Functions
#'
#' @noRd
mod_plot_mutational_signatures_server <- function(id, mutalisk_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mutalisk_validated_df <- reactive({
      validate(need(!is.null(mutalisk_df()), message = "Please select a dataset"))
      mutalisk_df()
    })
    
    columnnames <- reactive({
      mutalisk::mutalisk_dataframe_metadata_column_names(mutalisk_validated_df())
    })
    
    observeEvent(mutalisk_df(), {
      shiny::updateSelectInput(inputId = "in_select_facet_column", choices = c("No faceting", columnnames()))
    })
    
    selected_facet_column <- reactive({
      if(is.null(input$in_select_facet_column) | input$in_select_facet_column == "No faceting")
        return(NA)
      else
        return(input$in_select_facet_column)
    })
    
    plot_stacked_bar <- reactive({ function() { 
      print(mutalisk::plot_stacked_bar(
        mutalisk_dataframe = mutalisk_validated_df(),
        lump_type = input$in_select_lump_type,
        facet_column = selected_facet_column(),
        legend = input$in_select_legend, legend_direction = "horizontal",
        #facet_ncol = input$in_num_facet_ncol,
        fontsize_strip = input$in_num_fontsize_strip,
        fontsize_axis_title = input$in_num_fontsize_axis_titles, 
        lump_min = input$in_num_lump_min,
        topn = input$in_num_topn
      ))
    } 
    })
    
    output$out_plot_stacked_bar <- renderPlot({
      tryCatch({
        plot_stacked_bar()()
      }, error = function(e){
        validate(paste0(as.character(e), collapse="; "))
      })
    })
    
    moduleDownloadPlotServer(id = "mod_download", session_parent = session, plotOutputId = "out_plot_stacked_bar", plotting_function = plot_stacked_bar(), default_filename = "mutsig_stacked_bar")
  })
}

## To be copied in the UI
# mod_plot_mutational_signatures_ui("plot_mutational_signatures_ui_1")

## To be copied in the server
# mod_plot_mutational_signatures_server("plot_mutational_signatures_ui_1")
