#' plot_heterogeneity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_heterogeneity_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("out_plot_heterogeneity")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    
    br(),
    hr(),
    shinyWidgets::panel(
      heading = "Options",
      htmlOutput(outputId = ns("out_html_found_vaf_column")),
      conditionalPanel(
        condition = "output.failed_to_automatically_find_vaf_column", ns = ns,
        mod_select_maf_column_ui(id = ns("mod_select_vaf_column"), label = "Select VAF column")
      ),
      shinyWidgets::awesomeCheckbox(inputId = ns("in_check_useSyn"), label = "Use Synonymous", value = FALSE, status = "default"),
      numericInput(inputId = ns("in_num_vaf_min"), label = "Minimum VAF threshold", value = 0, min = 0, max = 1, step = 0.05),
      numericInput(inputId = ns("in_num_vaf_max"), label = "Minimum VAF threshold", value = 1, min = 0, max = 1, step = 0.05),
      moduleDownloadPlotUI(id = ns("mod_download_plot"))
    )
  )
}

#' plot_heterogeneity Server Functions
#'
#' @noRd 
mod_plot_heterogeneity_server <- function(id, maf, tsb){
  utilitybeltshiny::assert_reactive(maf)
  utilitybeltshiny::assert_reactive(tsb)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Reactive Validation -----------------------------------------------------
    maf_validated <- reactive({ validate(need(!is.null(maf()),message = "Loading ..." )); return(maf()) })
    tsb_validated <- reactive({ validate(need(!is.null(tsb()),message = "Loading ..." )); return(tsb()) })
    in_check_useSyn <- reactive({ validate(need(!is.null(input[["in_check_useSyn"]]),message = "Loading ..." )); return(input[["in_check_useSyn"]]) })
    
    
    # Reactive Vals -----------------------------------------------------------
    failed_to_automatically_find_vaf_column <- reactiveVal(FALSE)
    
    # Try Automatically Finding VAF column ------------------------------------
    het_auto =  reactive({
      #browser()
      #browser()
      het_ <- tryCatch(
        expr = { 
          maftools::inferHeterogeneity(maf = maf_validated(), tsb = tsb_validated(), useSyn = in_check_useSyn(), minVaf = input$in_num_vaf_min, maxVaf = input$in_num_vaf_max)
        },
        error = function(err){
          err=as.character(err)
          if(stringr::str_detect(string = err, pattern = "Use vafCol to manually specify vaf column name")){
            #validate("Can't automatically identify column specifying the vaf. Please manually specify the vaf column name")
            return(NULL)
          }
          else{
            utilitybeltassertions::fmterror(err)
            return(NULL)
          }
        }
      )
      
      if(is.null(het_)){
        failed_to_automatically_find_vaf_column(TRUE)
        #browser()
        
        het_ <- tryCatch(
          expr = { 
            #ADD SEGFILE DATA at some point
            #ADD ignore Chromosomes
            maftools::inferHeterogeneity(maf = maf_validated(), tsb = tsb_validated(), useSyn = in_check_useSyn(), vafCol = vaf_column(), minVaf = input$in_num_vaf_min, maxVaf = input$in_num_vaf_max) %>% 
              return()
          },
          error = function(err){
            err=as.character(err)
            if(stringi::stri_detect(err, regex = "object .*not found")){
             validate("Wrong VAF column selected. Try another") 
            }
            validate(err)
          }
        )
        
        #There are some ways thet inferHeterogeneity fails with a message but no error and just returns NULL. Must catch these
        if(is.null(het_)){
          fail_messages <- utils::capture.output(
            type= "message",
            maftools::inferHeterogeneity(maf = maf_validated(), tsb = tsb_validated(), useSyn = in_check_useSyn(), vafCol = vaf_column(), minVaf = input$in_num_vaf_min, maxVaf = input$in_num_vaf_max) %>%
              return()
          ) %>%
            paste0(collapse = "")
          
          if(stringi::stri_detect(fail_messages, regex = "Too few mutations for clustering")){
           validate("Either the wrong VAF column was selected or there is just too few mutations for clonal heterogeneity analysis") 
          }
          else
            validate(fail_messages)
        }
      }
      else
        failed_to_automatically_find_vaf_column(FALSE)
      
      return(het_)
      
      
    })
    
    
    # Was automatic crawling successful? --------------------------------------
    #failed_to_automatically_find_vaf_column = reactive({ is.null(het_auto()) })
    output[["failed_to_automatically_find_vaf_column"]] <- reactive({(failed_to_automatically_find_vaf_column())})
    outputOptions(output, "failed_to_automatically_find_vaf_column", suspendWhenHidden = FALSE) 
    
    # Manaully Select VAF Column ----------------------------------------------------------
    vaf_column_unvalidated <- mod_select_maf_column_server(id = "mod_select_vaf_column", maf = maf)
    vaf_column <- reactive({ validate(need(!is.null(vaf_column_unvalidated()),message = "Loading ..." )); return(vaf_column_unvalidated()) })
    
    # het_manual <- reactive({
    #   maftools::inferHeterogeneity(maf = maf_validated(), tsb = tsb_validated(), useSyn = in_check_useSyn(), vafCol = vaf_column_final()) 
    #   })
    
    
    
    #output$failed_to_automatically_find_vaf_column <- failed_to_automatically_find_vaf_column()
    
    output$out_html_found_vaf_column <- renderText({
      
      if(failed_to_automatically_find_vaf_column()){
        tags$p("We couldn't automatically find the column that contains VAFs. Please manually specify it", class="text-warning") %>%
          as.character() %>%
          return()
      }
      else{
        tags$p("VAF column automatically identified", class="text-success") %>%
          as.character() %>%
          return()
      }
    })
    
    
    # Plotting function -------------------------------------------------------
    plot_heterogeneity <- reactive({
      # browser()
      #browser()
      function(){
        #clusters= ifelse(failed_to_automatically_find_vaf_column(), yes=het_auto(), no = het_auto())
        #clusters = het_auto()
        maftools::plotClusters(clusters = het_auto())
      }
    })
    
    output$out_plot_heterogeneity <- renderPlot({ 
      plot_heterogeneity()()
    })
    
    filename <- reactive({ paste0(tsb_validated(),"_clonality") })
    
    moduleDownloadPlotServer(id = "mod_download_plot", session_parent = session, plotOutputId = "out_plot_heterogeneity", plotting_function = plot_heterogeneity(), default_filename = filename())
  })
}

## To be copied in the UI
# mod_plot_heterogeneity_ui("plot_heterogeneity_ui_1")

## To be copied in the server
# mod_plot_heterogeneity_server("plot_heterogeneity_ui_1")
