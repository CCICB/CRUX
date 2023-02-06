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

