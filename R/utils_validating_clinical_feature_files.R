#' Is a clinical_feature_file valid for a given maf
#'
#' @description
#' Runs read.maf with a given clinicalData and maf object pair and returns TRUE ONLY if the clinicalData file is appropriate for the given MAF.
#' If maf object is not valid, this function will also return FALSE.
#' By using clinicalData=NULL the function allows testing validity of a MAF file in Isolation.
#'
#' @inheritParams maftools::read.maf
#'
#' @return True / False (boolean)
#' 
#' @details 
#' 
#' If clinical feature file has duplicated Tumor_Sample_Barcodes, only the first entry is considered and the file is still 'valid'.
#' 
#'
is_valid_clinicalfeaturefile <- function(clinicalData, maf){
  assertthat::assert_that(!shiny::is.reactive(clinicalData))
  assertthat::assert_that(!shiny::is.reactive(maf))
  #browser()
  clinicalfeaturefile_is_valid <- tryCatch(
    expr = { 
      new_maf <-  maftools::read.maf(maf = maf, clinicalData = clinicalData, verbose = FALSE); return(TRUE) 
    },
    error = function(err){
      message(utilitybeltassertions::fmtwarning("Clinical feature file is not valid. ERROR: ", err))
      return(FALSE)
      
    },
    warning = function(warn){
      message(utilitybeltassertions::fmtwarning("Clinical feature file is not completeley valid. ERROR: ", warn))
      return(FALSE)
    }
  )
  
  assertthat::assert_that(assertthat::is.flag(clinicalfeaturefile_is_valid))
  return(clinicalfeaturefile_is_valid)
}



#' Is a clinical_feature_file valid for a given maf
#'
#' @description
#' Runs read.maf with a given clinicalData and maf object pair and returns the relevant error messages
#' If maf object is not valid, this function will also return the appropriate error message.
#' By using clinicalData=NULL the function allows testing validity of a MAF file in isolation
#'
#' @inheritParams maftools::read.maf
#'
#' @return Metadata and MAF are valid if they are. The relevant string if they are not (character)
#' 
#' @details 
#' 
#' If clinical feature file has duplicated Tumor_Sample_Barcodes, only the first entry is considered and the file is still 'valid'.
#' 
#'
is_valid_clinicalfeaturefile_return_error <- function(clinicalData, maf){
  assertthat::assert_that(!shiny::is.reactive(clinicalData))
  assertthat::assert_that(!shiny::is.reactive(maf))
  #browser()
  warning_or_error_messages <- tryCatch(
    expr = { 
      new_maf <-  maftools::read.maf(maf = maf, clinicalData = clinicalData, verbose = FALSE); return("Clinical Feature File and MAF are both valid") 
    },
    error = function(err){
      return(as.character(err))
      
    },
    warning = function(warn){
      return(as.character(warn))
    }
  )
  
  return(warning_or_error_messages)
}
