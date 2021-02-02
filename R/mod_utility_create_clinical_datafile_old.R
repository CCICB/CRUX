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
          shinyWidgets::awesomeCheckbox(inputId = ns("in_checkbox_map_source_column"),label = "My MAF was merged and I want to know the names of the source files", value = F),
          textInput(inputId = ns("in_text_feature_name"), label = "New feature name", placeholder = "feature 1", value = NULL),
          uiOutput(outputId = ns("out_actionbttn_add_feature")),
          ),
    shinyWidgets::panel(heading = "Clinical Data Table", 
            rhandsontable::rHandsontableOutput(ns("out_rt_clinical_data"), height = "300px", width = "100%")
          ),
    shinyWidgets::panel(heading = "Download", 
          #moduleDownloadDatatableUI(id=ns("mod_download_clinical_features"))
          )
    )
  }

#Takes maf file (reactive object), returns nothing.
moduleCreateClinicalDatafileServer <- function(id, maf){
  moduleServer(id,
    function(input, output, session){
    
      clinical_data <- reactiveVal()
    
     observe({
       validate(need(!is.null(maf()), message = "Please supply an MAF file"))
        clinical_features <- maftools::getClinicalData(maf())
        clinical_data(clinical_features)
        })
     
     observeEvent(input$in_checkbox_map_source_column,  {
       old_data <- clinical_data()
       if(input$in_checkbox_map_source_column == T & !all(colnames(sample_to_source_map()) %in% colnames(old_data)))
         clinical_data(base::merge(x= old_data, y=sample_to_source_map(), all.x=T, by="Tumor_Sample_Barcode"))
       else if (input$in_checkbox_map_source_column == F)
         clinical_data(old_data %>% dplyr::select(-colnames(sample_to_source_map()[2])))
     })
     
      observe({ message("button: ", input$in_actionbttn_add_feature) })
      
      sample_to_source_map <- reactive({
        validate(need("Source_MAF" %in% colnames(maf()@data), "Cannot add source column as maf was never merged from filepaths. Try merging using the utilities/merge tab"))
        validate(need(!any(duplicated(maf()@data %>% dplyr::group_by(Tumor_Sample_Barcode) %>% summarize(Source_MAF=unique(Source_MAF)) %>% dplyr::pull(Tumor_Sample_Barcode))) , "Cannot add source as clinical feature as samples are not unique across source mafs"))
        maf()@data %>% dplyr::group_by(Tumor_Sample_Barcode) %>% summarize(Source_MAF=unique(Source_MAF)) %>% return()
        })
      
      
      feature_name_is_legal <- reactive({ return(!is.null(input$in_text_feature_name) & input$in_text_feature_name != "")})
      
      observeEvent(eventExpr = input$in_actionbttn_add_feature, handlerExpr = {
        validate(need(feature_name_is_legal(), message = "Input Valid Featurename"))
        validate(need(!is.null(clinical_data()), message = "Input Valid MAF"))
        
        oldvalue=clinical_data()
        clinical_data(oldvalue %>% dplyr::mutate("{input$in_text_feature_name}" := character(length = nrow(oldvalue))))
        updateTextInput(session = session, inputId = "in_text_feature_name", value = "")
        })
      
      # observeEvent(eventExpr = input$in_checkbox_map_source_column, {
      #   oldvalue=clinical_data()
      #   clinical_data(merge(x= oldvalue, y=sample_to_source_map(), all.x=T, by="Tumor_Sample_Barcode"))
      # })

      clinical_data_handson <- reactive({ 
        validate(need(!is.null(clinical_data()), message = "Please supply an MAF file"))
        rhandson_out <- rhandsontable(clinical_data(), useTypes = F) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = TRUE)
        #clinical_data(rhandson_out)
        return(rhandson_out)
        })
      
      
      clinical_data_displayed <- reactive({ hot_to_r(input$out_rt_clinical_data) })
      
      
      add_feature_button_tooltip <- reactive({ if(feature_name_is_legal()) return("") else  return("Please enter a valid feature name") })
      add_feature_button_color <- reactive({if(feature_name_is_legal()) return("success") else return("danger") })
      output$out_actionbttn_add_feature <- renderUI({ shinyBS::tipify(shinyWidgets::actionBttn(inputId = session$ns("in_actionbttn_add_feature"), label = "Add Feature", size = "sm", color = add_feature_button_color()), 
                                                           title = add_feature_button_tooltip(), placement = "right") })
      
    output$out_rt_clinical_data <- rhandsontable::renderRHandsontable({clinical_data_handson()})
    moduleDownloadDatatableServer(id = "mod_download_clinical_features", data_to_write = clinical_data_displayed(), filename_full = "clinical_features.tsv", rownames = F, colnames = T)
  }
  )
}

# Copy in UI
# moduleCreateClinicalDatafileUI("some_id")

# Copy in server
# moduleCreateClinicalDatafileServer("some_id", optional_argument)
