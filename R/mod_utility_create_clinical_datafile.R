#Returns taglist of UI elements.
moduleCreateClinicalDatafileUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::panel(heading = "Instructions",
          tags$ol(
            tags$li("Import a dataset using the", tags$b( "Import MAF " ), "sidebar panel"),
            tags$li("Write a name of the clinical feature you want add in the", tags$b(" New feature name "), "field"),
            tags$li("Click", tags$b(" Add Feature ")),
            tags$li("To delete a feature:",tags$b(" Right Click Column => Delete Column ")),
            tags$li("When happy with your table, download the file then import it using the", tags$b(" Import clinical feature file "), "sidebar panel")
          )
          ),
    shinyWidgets::panel(heading = "Options",
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_map_source_column"),label = "My MAF was previously merged and I want to know the names of the source files", value = F),
          textInput(inputId = ns("in_text_feature_name"), label = "New feature name", placeholder = "New Feature Name", value = NULL),
          uiOutput(outputId = ns("out_actionbttn_add_feature")),
          ),
    shinyWidgets::panel(heading = "Added Features (click to delete)",
          uiOutput(outputId = ns("out_ui_remove_feature_bttns"))
      ),
    shinyWidgets::panel(
      heading = "Clinical Data Table", 
      fluidRow(
      rhandsontable::rHandsontableOutput(ns("out_rhandsontable_clinical_data"), height = "600px", width="80%")),
      excelOutput(ns("out_excel_clinical_data")),
      DT::renderDataTable(ns("out_excel_clinical_data2"), options = list(scrollX = TRUE), class = "display nowrap")
      ),
    shinyWidgets::panel(heading = "Download", 
          moduleDownloadDatatableUI(id=ns("mod_download_clinical_features"))
          )
    )
  }

#Takes maf file (reactive object), returns nothing.
moduleCreateClinicalDatafileServer <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
      
      sample_to_source_map <- reactive({
        validate(need("Source_MAF" %in% colnames(maf()@data), "Cannot add source column as maf was never merged from filepaths. Try merging using the utilities/merge tab"))
        validate(need(!any(duplicated(maf()@data %>% dplyr::group_by(Tumor_Sample_Barcode) %>% summarize(Source_MAF=unique(Source_MAF)) %>% dplyr::pull(Tumor_Sample_Barcode))) , "Cannot add source as clinical feature as samples are not unique across source mafs"))
        maf()@data %>% dplyr::group_by(Tumor_Sample_Barcode) %>% summarize(Source_MAF=unique(Source_MAF)) %>% return()
        })
      
      
      # Add feature -------------------------------------------------------------
      feature_name_is_legal <- reactive({ 
        return(!is.null(input$in_text_feature_name) & input$in_text_feature_name != "" & !input$in_text_feature_name %in% added_features_vec())
        })
      add_feature_button_tooltip <- reactive({ if(feature_name_is_legal()) return("") else  return("Please enter a valid feature name") })
      add_feature_button_color <- reactive({if(feature_name_is_legal()) return("success") else return("danger") })
      output$out_actionbttn_add_feature <- renderUI({ shinyBS::tipify(shinyWidgets::actionBttn(inputId = session$ns("in_actionbttn_add_feature"), label = "Add Feature", size = "sm", color = add_feature_button_color()), 
                                                           title = add_feature_button_tooltip(), placement = "right") })
      

      # Add features on buttonpress ---------------------------------------------
      added_features_vec <- reactiveVal(character(0))
      observeEvent(input$in_actionbttn_add_feature,{
        if(feature_name_is_legal()){
          added_features_vec(c(added_features_vec(),input$in_text_feature_name))
        }
        updateTextInput(inputId = "in_text_feature_name", session = session, value = "")
        })
      

      # For each feature, add a button that when clicked will delete it ---------
      #Button UI
      remove_feature_buttons_list <- reactive({
        buttons_ls <- list()  
        for (feature in added_features_vec()) {
          buttons_ls[[feature]] <- shinyWidgets::actionBttn(inputId = session$ns(feature), label = feature, size = "xs") %>%
            shinyBS::tipify(title = paste0("Remove ", feature))
        }
        print(buttons_ls)
        return(buttons_ls)
        })
      #Button Actions
      observe({
        button_actions <- list()  
        for (feature in added_features_vec()) {
          observeEvent(input[[feature]], {
            added_features_vec(added_features_vec()[added_features_vec() != feature])
            })
        }
      })
      
      # Remove features on buttonpress ------------------------------------------
      output$out_ui_remove_feature_bttns <- renderUI({
        remove_feature_buttons_list()
        })
      
      

    # Create and present clinical dataframe as excel-like table --------------------
      clinical_data_original_df <- reactive({ 
        validate(need(!is.null(maf()), "Please import MAF file"))
        return(maftools::getClinicalData(maf()))
        })
      

    # Resolve original and user-modified dataframes while adding colum --------
      clinical_data_original_with_added_features_df <- reactive({
        mydf = clinical_data_original_df()
        mydf[,added_features_vec()] <- character(nrow(mydf))
        
        #Overwrite original dataframe with values saved from user-modified version (so we don't undo everything they type)
        if(!is.null(clinical_data_user_modified_df())){
          mydf %>% colnames
          for (column in colnames(mydf)) {
            if(column %in% colnames(clinical_data_user_modified_df())) 
              mydf[[column]] <- clinical_data_user_modified_df()[[column]]
          }
        }
        
        #Add source column if maf was ever merged and user requests it
        if(input$in_checkbox_map_source_column == TRUE) {
          mydf <- base::merge(x= mydf, y=sample_to_source_map(), all.x=TRUE, by="Tumor_Sample_Barcode")
        }
        
        return(mydf)
        })
      
      renderExcelObject <- reactive({
        clinical_data_original_with_added_features_df() %>%
        excelTable(
          loadingSpin = TRUE,
          allowDeleteRow = FALSE, 
          allowInsertRow = FALSE, 
          allowInsertColumn = FALSE, 
          allowDeleteColumn = FALSE, 
          allowRenameColumn = FALSE, 
        )
      })
      # 
      # clinical_data_user_modified_df <- reactiveVal()
      # observeEvent(input$out_excel_clinical_data, {
      #   clinical_data_user_modified_df(excel_to_R(input$out_excel_clinical_data))
      #   print(clinical_data_user_modified_df())
      #   })
      
      # Render Table ------------------------------------------------------------
      output$out_excel_clinical_data <- renderExcel({
          renderExcelObject()
        })
      
      output$out_rhandsontable_clinical_data <- renderRHandsontable(
        rhandsontable(
          clinical_data_original_with_added_features_df(), useTypes = TRUE
          )
        )
      
      clinical_data_user_modified_df <- reactiveVal()
      observeEvent(input$out_rhandsontable_clinical_data, {
        clinical_data_user_modified_df(hot_to_r(input$out_rhandsontable_clinical_data))
        print(clinical_data_user_modified_df())
      })
      
      # 
      # 
      # user_modified_dataframe <- reactiveVal()
      # 
      # observe({
      #   user_modified_dataframe(excel_to_R(input$out_excel_clinical_data))
      #   print(excel_to_R(input$out_excel_clinical_data))
      # })
      
    

       
      
      #output$out_blabla <- DT::renderDataTable({ clinical_data_original_df() })
    # clinical_data_handson <- reactive({
    #   validate(need(!is.null(clinical_data()), message = "Please supply an MAF file"))
    #   rhandson_out <- rhandsontable(clinical_data(), useTypes = F) %>%
    #     hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
    #   #clinical_data(rhandson_out)
    #   return(rhandson_out)
    #   })
      
      
    #output$out_rt_clinical_data <- rhandsontable::renderRHandsontable({clinical_data_handson()})
    moduleDownloadDatatableServer(id = "mod_download_clinical_features", data_to_write = user_modified_dataframe(), filename_full = "clinical_features.tsv", rownames = F, colnames = T)
  }
  )
}

# Copy in UI
# moduleCreateClinicalDatafileUI("some_id")

# Copy in server
# moduleCreateClinicalDatafileServer("some_id", optional_argument)
