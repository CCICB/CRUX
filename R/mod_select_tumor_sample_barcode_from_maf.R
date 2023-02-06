#' mod_select_tumor_sample_barcode_from_maf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_tumor_sample_barcode_from_maf_ui <- function(id, label="Tumor Sample Barcode"){
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(inputId = ns("in_pick_tsb"), label = label,choices = c(), options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE))
  )
}

#' mod_select_tumor_sample_barcode_from_maf Server Functions
#'
#' @noRd 
mod_select_tumor_sample_barcode_from_maf_server <- function(id, maf){
  assertions::assert_reactive(maf)
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    maf_validated <- reactive({ validate(need(!is.null(maf()), message = "Waiting on MAF object to appear")); maf() })
    # tumor_sample_barcodes <- reactive({
    #   validate(need(!is.null(maf()), message = "Waiting on MAF object to appear"))
    #   tsbs <- unique(as.character(maftools::getSampleSummary(maf())[["Tumor_Sample_Barcode"]]))
    #   return(tsbs)
    # })
    # 
    tumor_sample_barcodes <- reactive({ 
      maf_validated() %>% 
        maftools_get_all_data(include_silent_mutations = TRUE) %>% 
        dplyr::count(Tumor_Sample_Barcode, sort = TRUE) %>% 
        dplyr::pull(Tumor_Sample_Barcode) %>%
        as.character() 
      })
    mutations <- reactive({ maf_validated() %>% 
        maftools_get_all_data(include_silent_mutations = TRUE) %>% 
        dplyr::count(Tumor_Sample_Barcode, sort = TRUE) %>% 
        dplyr::pull(n) 
      })
      
    
    observeEvent(tumor_sample_barcodes(), {
      isolate({
        shinyWidgets::updatePickerInput(session = session, inputId = "in_pick_tsb", choices = tumor_sample_barcodes(), choicesOpt = list(subtext = paste("Mutations (total)", mutations(), sep = ": ")))
      }) 
    })
    
    selected_tsb <- reactive({input$in_pick_tsb})
    
    return(selected_tsb)
    })
}


## To be copied in the UI
# mod_select_tumor_sample_barcode_from_maf_ui("mod_select_tumor_sample_barcode_from_maf_ui_1")

## To be copied in the server
# mod_select_tumor_sample_barcode_from_maf_server("mod_select_tumor_sample_barcode_from_maf_ui_1")
