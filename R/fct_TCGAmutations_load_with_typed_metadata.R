
#' Load TCGA data
#'
#' Load TCGA data with correctly typed clinical data. 
#' By default, TCGAmutations::tcga_load makes all MC3 dataset clinical features character vectors
#'
#' @inheritParams TCGAmutations::tcga_load
#'
#' @return MAF object with correctly typed clinical.data dataframe (MAF)
#' @export
#'
#' @examples
#' TCGAmutations_load_with_typed_metadata(study = "GBM")
TCGAmutations_load_with_typed_metadata <- function(study, source="MC3"){
  utilitybelt::assert_non_empty_string(study)
  utilitybelt::assert_non_empty_string(source)
  
  maf = TCGAmutations::tcga_load(study = study, source = source)
  maftools_fix_clinical_data_types(maf) %>%
    return()
}