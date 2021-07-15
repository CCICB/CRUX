
#' Download MAF
#'
#' Downloads a MAF file (inc. silent mutations). Does NOT download clinical datafile.
#'
#' @param maf a loaded maf object
#' @param file the path to save maf to 
#'
#' @return NOTHING. Run for its side effects
#'
#'
#' @examples
#' CRUX:::download_maf(TCGAmutations::tcga_load("GBM"), "tcga_gbm.maf")
download_maf <- function(maf, file){
  data.table::fwrite(
    x = data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE), 
    file = file, 
    sep = "\t", 
    quote = FALSE, 
    row.names = FALSE)
}


