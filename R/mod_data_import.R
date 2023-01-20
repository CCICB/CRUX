#' data_import2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  
  # Step 1
  tagList(
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 1: "), "Import Genomic Data (MAF)"),
      fluidRow(
        shiny::fileInput(inputId = ns("in_file_maf"), label = "Select MAF file") %>% col_3(),
        shinydashboard::box(
          title = "Mutation Annotation Format (MAF) Files",width = "100%",
          "
          MAF files are tabular files that storing a list of mutations. 
          To learn more about how to get your data in MAF format: see here.
          ") %>% column(width = 9)
      ),
    ), icon_down_arrow(break_after=TRUE),
    
    # Step 2
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 2: "), "Import Clinical Annotations"),
      fluidRow(
        shiny::fileInput(inputId = ns("in_file_clindata"), label = "Select Clinical Annotations File") %>% col_3(),
        shinydashboard::box(
          title = "Clinical Annotation Files", width = "100%",
          "
          Clinical data associated with each sample/Tumor_Sample_Barcode in MAF. Could be a csv/tsv file.
          To learn more about how to prepare your clinical annotations file: see here.
          ") %>% column(width = 9)
      ),
    ),icon_down_arrow(break_after=TRUE),
    
    # Step 3
    shinyWidgets::panel(
      heading = tags$span(tags$strong("Step 3: "), "Add Cohort Level Metadata"),
      shiny::fluidRow(
        shiny::textInput(inputId = ns("in_text_displayname"), label = "Display Name", placeholder = "High Grade Glioma" ,width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_shortname"), label = "Short Name", placeholder = "HGG", width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_data_source"), label = "Source", placeholder = "ZERO Program", width = "100%") %>% col_4(),
        shiny::textInput(inputId = ns("in_text_description"), label = "Descripton", placeholder = "High Grade Glioma from a paediatric cohort with survival <30%", width = "100%") %>% col_12()
      )
    ),icon_down_arrow(break_after=TRUE),
    
    # Step 4
    shinyWidgets::panel(
      heading = "Step 4: Import Data!",
      shiny::actionButton(
        inputId = ns("in_bttn_import"), 
        label = "Import", 
        width = "100%",
        icon = icon("file-import"),
        class = "btn btn-primary"
      )
    )
  )
}

#' data_import2 Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id, maf_data_pool){
  utilitybeltshiny::assert_reactive(maf_data_pool)
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Define some basic reactive values
    cohort_metdata_filled <- reactive({
      nchar(input$in_text_displayname) > 0 & 
      nchar(input$in_text_shortname) > 0 &
      nchar(input$in_text_data_source) > 0 &
      nchar(input$in_text_description) > 0 
      })
    
    
    # When clicking the button 'add to data pool'
    observeEvent(input$in_bttn_import, isolate({
      
      
      # Check a maf file has been supplied
      if(is.null(input[["in_file_maf"]]$datapath)){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Missing MAF",
          text = "Please select a MAF file",
          type = "warning"
        )
        return(NULL)
      }
      
      # Check all metadata has been supplied
      if(!cohort_metdata_filled()){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Missing metadata",
          text = "Fill out all cohort level metadata fields",
          type = "warning"
        )
        return(NULL)
      }
      
      
      # Try and read maf file
      # If it fails, save error message to maf variable instead
      maf <- tryCatch({
        maftools::read.maf(maf = input[["in_file_maf"]]$datapath, clinicalData = input[["in_file_clindata"]]$datapath)
        }, error = function(err) return(as.character(err)), warning = function(warn) {return(as.character(warn))} )
      
      # If MAF read failed, inform user of error
      if(is.character(maf)){
        message <- if(!is.null(input[["in_file_clindata"]])) "and clinical metadata " else " "
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Failed to Read MAF",
          text = tags$div(
            "Please ensure MAF file ", message,"is formatted correctly.", tags$br(), tags$br(), tags$code(maf)),
          html = TRUE,
          type = "warning"
        )
        return(NULL)
      }
      
      # Add to data pool
      updated_maf_data_pool <- user_to_dataset_to_data_pool(
        maf_data_pool = maf_data_pool(), 
        filepath = input[["in_file_maf"]]$datapath,
        display_name = input[["in_text_displayname"]], 
        short_name = input[["in_text_shortname"]], 
        description = input[["in_text_description"]],
        data_source = input[["in_text_data_source"]],
        loaded_data = maf
      )
      maf_data_pool(updated_maf_data_pool)
      
      # Send success message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Dataset Succesfully Added",
        text = "Go to 'Single Cohort Genomics' module and select your new dataset to start deriving insights!",
        type = "success"
      )
      }))
    
  })
}
