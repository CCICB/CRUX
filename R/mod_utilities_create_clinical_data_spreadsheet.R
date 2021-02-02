# UI ------------------------------------------------------------------
#' utilities_create_clinical_data_spreadsheet UI Function
#'
#' @description Provides an excel-like gui interface for adding a clinical feature file to a maf object. 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_utilities_create_clinical_data_spreadsheet_ui <- function(id){
  ns <- NS(id)
  tags$style(".fa-check {color:#69C776}")
  tags$style(".fa-times {color:#ED7676}") 
  
  tagList(
    tags$style(paste0("#", ns("in_text_add_feature"),  " fa-check {color:#69C776}")),
    shinyWidgets::panel(heading = "Options",
      shinyWidgets::textInputIcon(inputId = ns("in_text_add_feature"), label = "Add Feature", placeholder = "Feature Name", icon = list(NULL, icon(name="check"))),
      actionButton(inputId = ns("in_bttn_add_feature"), label = "Add Feature")
      ),
    
    shinyWidgets::panel(heading = "Add your metadata",
                        
      div(style ="height: 500px; width: 100%; overflow-y: scroll",
        excelR::excelOutput(outputId = ns("out_excelr_clinical_features"), height="100%"),
      )
    ),
    
    shinyWidgets::panel(
      heading="Debug",
      DT::dataTableOutput(outputId = ns("out_dt_latest_df")) %>% shinycssloaders::withSpinner(),
      shinyWidgets::actionBttn(ns("in_bttn_browser"), label="browser")
    ),
    
    
    shinyWidgets::panel(heading="Save metadata for use in future sessions",
        
        utilitybeltshiny::mod_download_dataframe_ui(id = ns("mod_download_clinical_feature_df"), label = "Download", tooltip_text = "Exports the metadata as a TSV file that can loaded in future sessions", tooltip_pos = "right", style = "fill", size = "md")
      )
    )
}


# Server ------------------------------------------------------------------
#' utilities_create_clinical_data_spreadsheet Server Functions
#'
#' Takes a maf object (reactive) and returns a dataframe with the 
#'
#' @noRd 
mod_utilities_create_clinical_data_spreadsheet_server <- function(id, maf){
  
  moduleServer(id, function(input, output, session){
      
      my_maf <- reactive({
        message("MAF changed")
        maf()
        })
    
      # Session Specific Variables ----------------------------------------------
      ns <- session$ns
      latest_saved_df = reactiveVal(NULL)
      
      # Assertions --------------------------------------------------------------
      utilitybelt::assert_that(is.reactive(maf), msg = utilitybelt::fmterror("mod_data_import_step3_server: maf should be reactive. Check you didn't accidentally add parenthesis when passing argument"))
      
      # REACTIVES --------------------------------------------------------
      maf_file_associated_df <- reactive({
        #message("MAF changed")
        #latest_saved_df(maftools::getClinicalData(maf()))
        maftools::getClinicalData(maf())
      }) #Clinical data already attached to the input MAF object. Always contains a column: tumor sample barcode
      
      observe({ 
        hot_table_to_df_value = excelR::excel_to_R(input$out_excelr_clinical_features) 
        if(!is.null(hot_table_to_df_value)){
         isolate(latest_saved_df(hot_table_to_df_value))
        }
        })
      
      feature_name_is_valid <- reactive({
        is_potential_feature_name_valid(input$in_text_add_feature, maf_file_associated_df())
      })
      
      
      excel_table <- reactive({
        validate(need(!is.null(latest_saved_df()), message = "Please Import MAF"))
        excelR::excelTable(latest_saved_df(), columns = data.frame(title=colnames(latest_saved_df()), readOnly = c(TRUE, rep(FALSE, times=ncol(latest_saved_df())-1))), editable = TRUE, allowInsertRow = FALSE, allowDeleteRow = FALSE, autoColTypes = TRUE, autoFill = TRUE,  autoWidth = FALSE, columnSorting = TRUE, allowInsertColumn = FALSE, allowRenameColumn = FALSE) %>% return() #Throws a warning saying its going to guess column type... cant figure out how to turn that off quite ye
      }) 
      
      final_df <- reactive({
        excelR::excel_to_R(input$out_excelr_clinical_features)
      })
      
      # OBSERVERS ---------------------------------------------------------------
        observeEvent(feature_name_is_valid(), {
          if(isolate(feature_name_is_valid()))
            shinyWidgets::updateTextInputIcon(session = session, inputId = "in_text_add_feature", icon = list(NULL, icon("check")))
          else
            shinyWidgets::updateTextInputIcon(session = session, inputId = "in_text_add_feature", icon = list(NULL, icon("times")))
        })
      
      # Trigger Events ----------------------------------------------------------
      observeEvent(input$in_bttn_add_feature, {
        if(isolate(feature_name_is_valid())){
          message("Adding feature")
          current_df = isolate(latest_saved_df())
          new_feature_name = isolate(input$in_text_add_feature)
          current_df[[new_feature_name]] <- character(nrow(current_df))
          isolate(latest_saved_df(current_df))
          updateTextInput(inputId = "in_text_add_feature", value = "", session = session)
        }
      })
      
      # Render Final Excel_table ------------------------------------------------
      output$out_excelr_clinical_features <- excelR::renderExcel({
        excel_table()
      })
      
      
      # Download ----------------------------------------------------------------
      utilitybeltshiny::mod_download_dataframe_server(id = "mod_download_clinical_feature_df", data_to_write = latest_saved_df, rownames = FALSE, colnames = TRUE, filename_full = "clinical_feature_file.tsv")
      
      
      # debug -------------------------------------------------------------------
      observeEvent(input$in_bttn_browser, {
        browser()
      })

      output$out_dt_latest_df <- DT::renderDataTable({ maftools::getClinicalData(my_maf()) }, options = list(scrollX = TRUE), class = "display nowrap")
      
      # Return Clinical Feature Dataframe ---------------------------------------
      return(latest_saved_df)

  })
}



# Helper Functions ---------------------------------------------------------------
is_potential_feature_name_valid <- function(feature_name, dataframe){
  utilitybelt::assert_that(is.data.frame(dataframe), msg = utilitybelt::fmterror("is_potential_feature_name_valid: dataframe must be data.frame NOT a ", class(dataframe)))
  
  is_nonzero_string = nzchar(feature_name)
  is_already_in_dataframe = feature_name %in% colnames(dataframe)
  
  if(is_nonzero_string & !is_already_in_dataframe){
   return(TRUE)
  }
  else
    return(FALSE)
}

## To be copied in the UI
# mod_utilities_create_clinical_data_spreadsheet_ui("utilities_create_clinical_data_spreadsheet_ui_1")
    
## To be copied in the server
# mod_utilities_create_clinical_data_spreadsheet_server("utilities_create_clinical_data_spreadsheet_ui_1")
