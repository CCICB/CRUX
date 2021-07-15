
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
TCGAmutations_load_with_typed_metadata <- function(study, source="Firehose"){
  utilitybeltassertions::assert_non_empty_string(study)
  utilitybeltassertions::assert_non_empty_string(source)
  
  maf = TCGAmutations::tcga_load(study = study, source = source)
  maftools_fix_clinical_data_types(maf) %>%
    maftools_fix_tcga_survival_curve_metadata() %>%
    return()
}
