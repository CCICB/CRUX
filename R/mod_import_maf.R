
#' Read MAF UI function
#'
#' @param id links ui and server components of module
#' @param panel_name "panel name (required)"
#' @param cohort_name "panel name (required)"
#' @param label_cohort_name "name of the cohort (required)"
#' @param label_name "label of the cohort name field"
#' @param label_maf label of maf input field ["maf"]
#' @param label_clinical_data label of clinicalData input field ["clinical data"]
#'
#' @return
#' @export
#'
moduleReadMafUI <- function(id, panel_name, cohort_name, label_cohort_name = "name", label_maf="maf", label_clinical_data="clinical features") {
  ns <- NS(id)
  
  tagList(
    shinyWidgets::panel(heading = panel_name,
      
      shinyBS::tipify(
        shinyWidgets::panel(status = "default", heading ="Step 1: Name Experiment", textInput(inputId = ns("in_text_cohort_name"), label = label_cohort_name, value = cohort_name, placeholder = cohort_name)),
        title = "Name of experiment", placement = "right", trigger = "hover",  options=list(container="body")),
      
      shinyBS::tipify(
      shinyWidgets::panel(status = "default", heading ="Step 2: Import MAF", fileInput(inputId = ns("in_file_maf"), label = label_maf, accept = c(".gz", ".maf"))),
      title = "Import MAF file. If you are unsure about how to format your data, see FAQ", placement = "right", trigger = "hover", options=list(container="body")),
      
      
      shinyBS::tipify(
        shinyWidgets::panel(status = "default", heading ="Step 3: Import clinical feature file", fileInput(inputId = ns("in_file_clinicalData"), label = label_clinical_data, accept = c(".tsv", ".txt"))),
        title = paste0("A tab-delimited file associating categorical variables with their sample names in the MAF file. This file can be created in ", tags$b("utilities/Add Clinical Data")), placement = "right", trigger = "hover", options=list(container="body")
      ),
      
      #shinyBS::bsTooltip(),
      shinyBS::bsTooltip(id = ns("in_file_maf"), title = "Import MAF ", placement = "right", trigger = "hover", options = NULL),
      shinyBS::bsTooltip(id = ns("in_file_clinicalData"), title = "", placement = "bottom", trigger = "hover", options = NULL)
      
    )
  )
}



#' Read MAF server function
#'
#' @param id 
#'
#' @return named list containing 'maf' (type maf), 'cohortName' (type string), 'clinicalData' (type df), 'clinicalDataSupplied' (bool),
#' @export
#'
moduleReadMafServer <- function(id) {
  moduleServer(
    id = id,
    
    function(input, output, session) {
      output$out_text_maf_status <- renderText({return("warning")})
      
      in_file_maf <- reactive({ input$in_file_maf })
      in_file_clinical_data <- reactive({ input$in_file_clinicalData })
      clinical_data_supplied <- reactive({ !is.null(in_file_clinical_data()) })
      clinical_data_df <- reactive({ validate(need(clinical_data_supplied(), message = "No clinical data file supplied")); read.csv(in_file_clinical_data()$datapath, header = T, sep = "\t")})
      cohort_name <- reactive({ validate(need(input$in_text_cohort_name != "", message = "Please enter a valid cohort name")); return(input$in_text_cohort_name) })
      #browser()
      maf <- reactive({ 
        shiny::validate(need(!is.null(in_file_maf()), message = "Please import dataset" )) 
        
        if(clinical_data_supplied()) {
          validate(need("Tumor_Sample_Barcode" %in% colnames(clinical_data_df()), "Clinical feature file MUST include a column named Tumor_Sample_Barcode which contains your sample names"))
          maf_with_clinical <- read.maf(maf = in_file_maf()$datapath, clinicalData = clinical_data_df())
          #validate(need(nrow(maftools::getClinicalData(maf_with_clinical)), ""))
          sample_names <-maftools::getSampleSummary(maf_with_clinical) %>% dplyr::pull(Tumor_Sample_Barcode)
          missing_samples <- clinical_data_df() %>% dplyr::filter(!(Tumor_Sample_Barcode %in% sample_names)) %>% dplyr::pull(Tumor_Sample_Barcode)
          validate(need(clinical_data_df() %>% dplyr::pull(Tumor_Sample_Barcode) %in% sample_names %>% any, paste0("Clinical feature file is missing samples: ", paste0(missing_samples, collapse = ","))))
          return(maf_with_clinical)
        }
        else 
          return(read.maf(maf = in_file_maf()$datapath))
        })
      return(list(maf = maf, cohortName = cohort_name, clinicalData = clinical_data_df, clinicalDataSupplied = clinical_data_supplied))
    }
  )
}

# Copy in UI
# readMaf_UI("readMafUI")

# Copy in server
# callModule(readMaf_UI"readMafUI")
