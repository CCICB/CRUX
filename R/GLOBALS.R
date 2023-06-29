utils::globalVariables(
  c(".", "Abbreviation", "AlteredSamples", "Chromosome", "Cytoband", 
    "End_Position", "FILTER", "HGNC", "Hugo_Symbol", "ID", "INFO", 
    "Pathway", "QUAL", "Reference_Allele", "Sample Name", "Samples", 
    "Start_Position", "Strand", "TPM", "Tumor_Sample_Barcode", "Tumor_Seq_Allele2", 
    "V1", "Val", "aa.length", "chromStart", "dev.off", "everything", 
    "genes in wide peak", "input", "maf", "mod_plot_tsne_server", 
    "mod_plot_tsne_ui", "mod_select_dataset_from_maf_data_pool_pickerinput_ui", 
    "mod_select_datasets_from_maf_data_pool_server", "mod_select_datasets_from_maf_data_pool_ui", 
    "n", "name", "path_to_amp_genes_file", "pdf", "plotmafSummary", 
    "png", "protein.ID", "read.maf", "reference", "refseq.ID", 
    "slotNames", "svg", "tiff", "tool_name", "unique_name", "value") 
)
  

#' My function
#'
#' This is a description of my function.
#'
#' @import grDevices
#' @import methods
#' @import stats
#' @import utils
imports <- function() {
}

curly <- function(){
  curl::curl_escape('bob') 
}