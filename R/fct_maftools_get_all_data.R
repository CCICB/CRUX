#' Get MAF data 
#'
#' Input a maf object. Returns table containing ALL MAF data. Nonsynonymous AND synonymous
#'
#' @param maf 
#' @param include_silent_mutations 
#'
#' @return a data.frame in MAF form where each variant has a separate row (data.frame)
#' @export
#'
#' @examples
#' maftools_get_all_data(TCGAmutations::tcga_load("GBM"))
maftools_get_all_data <- function(maf, include_silent_mutations=T) {
  if(include_silent_mutations)
    data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE) %>% return()
  else
    return(maf@data)
}



#' Maftools rainfallPlot Wrapper
#'
#' Wraps maftools::rainfallPlot. The original function plots a rainfall graph, prints the predicted kataegis coords as a df, and writes it to a file.
#' This function does all of this EXCEPT it hides the printed df, reads the df from the file and returns it. It also deletes the created file.
#'
#' @inheritParams maftools::rainfallPlot
#' @param tsb specify sample names (Tumor_Sample_Barcodes) (string)
#'
#' @return predicted kataegis sites (dataframe)
#' @export
#' 
maftools_plot_rainfall <- function(maf, tsb, detectChangePoints = TRUE, ref.build = "hg19", pointSize = 0.4, fontSize = 1.2){
  utilitybelt::assert_that(!is.null(tsb) | detectChangePoints == FALSE, msg = "[maftool_rainfall_kaetagis_table] this wrapper function ONLY allows tsb to be null IF detectChangePoints == FALSE. Otherwise it can't find the output file which is named after the tsv")
  #message("ref.build: ", ref.build)
  
  closeAllConnections()
  sink(nullfile())
  maftools::rainfallPlot(maf = maf, tsb = tsb, detectChangePoints = detectChangePoints, ref.build = ref.build, fontSize = fontSize, pointSize = pointSize)
  closeAllConnections()
  message("All done")
  expected_filename = paste0(tsb, "_Kataegis.tsv")
  
  if(detectChangePoints==TRUE && !is.null(tsb) && file.exists(expected_filename)){
    kataegis_df = data.table::fread(file = expected_filename)
    unlink(expected_filename)
    return(kataegis_df)
  }
  
  return(NULL)
}

