#' mod_data_import_step1 UI Function
#'
#' @description GUI for taking a filepath returning a maf object.
#' Includes a panel containing the MAF file summary or an error message if the supplied path does not point to a valid maf.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList 
mod_data_import_maf_path_to_maf_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinyWidgets::panel(
      heading = "Preview",
        htmlOutput(outputId = ns("out_text_error_message")),
        DT::dataTableOutput(outputId = ns("out_dt_maf_summary")) %>% shinycssloaders::withSpinner(),
        br()
        
    )
  )
}
    
#' mod_data_import_step1 Server Functions
#' 
#' @param maf_path Path to maf file  (string) (reactive) 
#' @inheritParams maftools::read.maf
#' @returns maf object if read was successful. NULL if it was not (MAF) (reactive)
#' 
mod_data_import_maf_path_to_maf_server <- function(id, maf_path, clinicalData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    utilitybeltshiny::assert_reactive(maf_path)
    utilitybeltshiny::assert_reactive(clinicalData)
    
    clinical_data_path <- reactive({
      if(identical(clinicalData(), character(0))){
       return(NULL) 
      }
      else
        return(clinicalData())
    })
    
    #maf is either a maf object or NULL if either clinical data or MAF object were invalid
    maf <- reactive({
      if(length(maf_path()) != 1) return(NULL)
      if(is.null(maf_path())) return(NULL)
      if(class(maf_path()) != "character") return(NULL)

      # browser()
      try_result <- tryCatch(
        expr = {
          maf_ <- maftools::read.maf(maf_path(), clinicalData = clinical_data_path())
          message(utilitybeltassertionsfmtsuccess("Maf File Read Succesfully"))
          return(maf_)
        },
        error = function(err){
          #Check if error is due to maf or clinical data file
          maf_is_valid = is_valid_clinicalfeaturefile(clinicalData = NULL, maf = maf_path())
          if(maf_is_valid)
            message(utilitybeltassertionsfmtwarning("Attempt to read clinicalData failed, returning null. Error message: \n", err))
          else 
            message(utilitybeltassertions::fmtwarning("Attempt to read maf failed, returning null. Error message: \n", err))
          
          return(NULL)
        },
        warning = function(warn){
          maf_is_valid = is_valid_clinicalfeaturefile(clinicalData = NULL, maf = maf_path())
          
          if(maf_is_valid)
            message(utilitybeltassertions::fmtwarning("Attempt to read clinicalData failed, returning null. Error message: \n", warn))
          else 
            message(utilitybeltassertions::fmtwarning("Attempt to read maf failed, returning null. Error message: \n", warn))
          return(NULL)
        }
      )
      return(try_result)
      })
    
    msg = reactive({
      if(!is.null(maf_path()) & length(maf_path()) == 1 & is.null(maf())){
        maf_is_valid = is_valid_clinicalfeaturefile(clinicalData = NULL, maf = maf_path())
        
        if(maf_is_valid){
          msg <- paste0(tags$strong(clinical_data_path()), " is not a valid clinical feature file. See help page for details on how this file should be structured")
        }
        else
          msg <- paste0(tags$strong(maf_path()), " is not a valid MAF file. See the manual for details on how this file should be structured")
      }
      else
        return(NULL)
      
      return(msg)
    })
    
    output$out_text_error_message <- renderText({
      HTML(msg())
      })

    maf_summary_dt <- reactive({
      validate(need(is.null(msg()), message = "Problem with imported files"))
      validate(need(!is.null(maf()), message = "Please import a valid MAF file"))
      return(maftools::getSampleSummary(maf()))
    })
    output$out_dt_maf_summary <-  DT::renderDataTable({
      maf_summary_dt()
    }, options = list(scrollX = TRUE), class = "display nowrap", filter = 'top')
                            
    return(maf)
  })
}
    
## To be copied in the UI
# mod_mod_data_import_maf_path_to_maf_ui("mod_data_import_maf_path_to_maf_ui_1")
    
## To be copied in the server
# mod_mod_data_import_maf_path_to_maf("mod_data_import_maf_path_to_maf_ui_1")
