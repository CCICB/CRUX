#' datapool_viewer UI Function
#'
#' @description A shiny Module.
#' 
#' Returns taglist with the data pool rendered as a dataTableOutput 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_datapool_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(
    shiny::wellPanel(p(strong(a("PCAWG", href="https://dcc.icgc.org/pcawg", target="_blank")), "and" , strong(a("TCGA", href="https://www.cancer.gov/about-nci/organization/ccg/research/structural-genomics/tcga", target="_blank"))," datasets are immediately available to you. To import your own data, use the ",strong("Import Dataset"), " tab."))
    ),
    shinyWidgets::panel(heading = "Available Datasets",
      DT::dataTableOutput(outputId = ns("out_dt_data_pool")) %>% shinycssloaders::withSpinner(proxy.height = "200px"),
    )#,
    #shinyWidgets::panel(textOutput(ns("out_text")))
  )
}


#' Programmatically create a Shiny input
#' 
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
shinyInput <- function(FUN, n, id, label, ...) {
  
  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(seq_len(n), function(i){
    as.character(FUN(paste0(id, i), label = if(length(label) > 1) label[i] else label, ...))
  }, character(1))
  
}

#' datapool_viewer Server Functions
#'
#' @noRd 
mod_datapool_viewer_server <- function(id, maf_data_pool, parent_session){
  assertions::assert_reactive(maf_data_pool)
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    maf_data_pool_df <- reactive({ 
      maf_data_df <- maf_data_pool_to_dataframe(maf_data_pool())
      
      #browser()
      maf_data_df[["Name"]] <- shinyInput(
        FUN = actionButton,
        n = nrow(maf_data_df),
        id = ns('cohortselectbutton__'),
        label = maf_data_df[["display_name"]],
        onclick = paste0('Shiny.setInputValue(\"', ns("select_button"), '\", this.id, {priority: \"event\"})')
      )
      #browser()
      maf_data_df <- dplyr::rename(maf_data_df, 
        #Name = "display_name",
        Abbreviation = "short_name",
        Source = "name_of_data_source",
        Description = "data_description",
        "Sample Size" = "number_of_samples",
        "Unique Name" = unique_name
        )
      maf_data_df <- maf_data_df[c("Name", "Abbreviation", "Sample Size", "Source", "Description", "Unique Name")]
      
      return(maf_data_df)
      })
    
    maf_data_pool_datatable <- reactive({
      DT::datatable(data = maf_data_pool_df(), options = list(scrollX = TRUE), class = "display nowrap", filter = 'top', escape = FALSE, selection = 'none')
      })
    
    #"PanCohortStatistics"
    unique_name_selected <- eventReactive(input$select_button, {

      # take the value of input$select_button, e.g. "button_1"
      # get the button number (1) and assign to selectedRow
      #browser()
      selectedRow <- as.numeric(strsplit(input$select_button, "__")[[1]][2])

      # get the value of the "Name" column in the data.frame for that row
      unique_name_selected <- maf_data_pool_df()[["Unique Name"]][selectedRow]
      #message("unique_name_selected")
      return(unique_name_selected)
    })
    
    # Change tab to single cohort & update pickerinput
    observeEvent(unique_name_selected(), {
      shinydashboard::updateTabItems(session = parent_session, inputId = "tabs", selected = "PanCohortStatistics")
    
      # inputId will need to update if we ever change any ids in the chain
      shinyWidgets::updatePickerInput(session = parent_session, inputId = "mod_pan_cohort_statistics-mod_picker_dataset-in_pick_dataset", selected = unique_name_selected())
      })
    
    #output$out_text <- renderText({unique_name_selected()})
    
    output$out_dt_data_pool <- DT::renderDataTable({maf_data_pool_datatable()})
    
    # output$out_dt_data_pool_network <- sigmajs::renderSigmajs({ maf_data_pool_network() })
  })
}



## To be copied in the UI
# mod_datapool_viewer_ui("datapool_viewer_ui_1")
    
## To be copied in the server
# mod_datapool_viewer_server("datapool_viewer_ui_1")
