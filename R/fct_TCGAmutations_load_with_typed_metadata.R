
#' Load TCGA data
#'
#' Load TCGA data with correctly typed clinical data. 
#' By default, TCGAmutations::tcga_load makes all Firehose dataset clinical features character vectors
#'
#' @inheritParams TCGAmutations::tcga_load
#'
#' @return MAF object with correctly typed clinical.data dataframe (MAF)
#'
#'
#' @examples
#' TCGAmutations_load_with_typed_metadata(study = "GBM")
TCGAmutations_load_with_typed_metadata <- function(study, source="Firehose"){
  utilitybelt::assert_non_empty_string(study)
  utilitybelt::assert_non_empty_string(source)
  
  maf = TCGAmutations::tcga_load(study = study, source = source)
  maftools_fix_clinical_data_types(maf) %>%
    maftools_fix_tcga_survival_curve_metadata() %>%
    return()
}

maftools_fix_tcga_survival_curve_metadata <- function(maf){
  #browser()
  if (all(c("days_to_last_followup", "vital_status", "days_to_death") %in% colnames(maf@clinical.data))){
    maf@clinical.data$days_to_last_followup <- ifelse(is.na(maf@clinical.data$days_to_last_followup) & maf@clinical.data$vital_status==1, yes = maf@clinical.data$days_to_death, no = maf@clinical.data$days_to_last_followup)
    return(maf)
  }
  else
    return(maf)
}