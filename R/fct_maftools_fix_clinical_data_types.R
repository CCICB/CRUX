#'  Type MAF clinical data
#'
#' Returns a MAF with correctly typed clinical data. 
#' By default, many maftools operations (e.g. subsetting) results in a loss of clinical data type (everything gets cast as characters).
#' This function can be run on any MAF to resolve this issue.
#'
#' @param maf a maf object (MAF)
#'
#' @return MAF object with correctly typed clinical.data dataframe (MAF)
#'
#'
#' @examples
#' maf = maftools::tcgaLoad(study = "GBM", source = "Firehose")
#' str(maftools::getClinicalData(maf)) 
#' new_maf = CRUX:::maftools_fix_clinical_data_types(maf)
#' str(maftools::getClinicalData(new_maf))
maftools_fix_clinical_data_types <- function(maf){
  assertions::assert_class(maf, "MAF")
  maf@clinical.data <- type.convert(maf@clinical.data, as.is=TRUE)
  return(maf)
}



#' add BRCA subtype clinical feature
#'
#' Checks if maf is BRCA and contains cols required to classify as Triple Negative / Not Triple Negative.
#' If so, adds a 'subtype' column describing whether its triple negative or not triple negative
#'
#' @param maf maftools maf object
#'
#' @return  maf object
#'
maftools_add_brca_subtype <- function(maf){
  
  clinical_feature_names <- colnames(maf@clinical.data)
  
  her2 = "lab_proc_her2_neu_immunohistochemistry_receptor_status"
  #her2 = "lab_procedure_her2_neu_in_situ_hybrid_outcome_type"
  progesterone = "breast_carcinoma_progesterone_receptor_status"
  estrogen = "breast_carcinoma_estrogen_receptor_status"
  
  
    if(all(c(her2, progesterone, estrogen) %in% clinical_feature_names)){
      her2_status <- maf@clinical.data[[her2]]
      pr_status <- maf@clinical.data[[progesterone]]
      er_status <- maf@clinical.data[[estrogen]]
      
      maf@clinical.data[['subtype']] <- dplyr::case_when(
        her2_status == "Negative" &  pr_status == "Negative" & er_status == "Negative" ~ "Triple Negative",
        her2_status == 'Positive' | pr_status == "Positive" | er_status == "Positive" ~ "Not Triple Negative",
        TRUE ~ "Ambiguous" # If any of the recepter/her2 statuses are indeterminate or equivocal
      )
    }
  return(maf)
}



maftools_set_difficult_aachanges_to_empty_strings <- function(maf){
  maf@data$Protein_Change <- ifelse(grepl(x=maf@data$Protein_Change, pattern = "\\*[A-Z]+\\*.*[0-9]"), yes =  "", maf@data$Protein_Change)
  return(maf)
}