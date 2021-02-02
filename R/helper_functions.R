
#' Get Width/Height of a rendered element
#'
#' @param output_id "ID of the plot/element who's width/height you want to know"
#' @param session "Current Session. Used to get namespacing and clientData information"
#' @param return_inches "Should we return width/height in pixels or inches"
#' @param dpi "dpi used to convert pixels to inches"
#'
#' @return "Width/Height in pixels/inches"
#' @export
#'
get_rendered_plot_width <- function(output_id, session, return_inches = F, dpi=70){
  index <- paste0("output_", session$ns(output_id), "_width")
  width <- session$clientData[[index]]
  
  if (return_inches) return(width/dpi)
  else return(width)
}

#' Get Width/Height of a rendered element
#'
#' @param output_id "ID of the plot/element who's width/height you want to know"
#' @param session "Current Session. Used to get namespacing and clientData information"
#' @param return_inches "Should we return width/height in pixels or inches"
#' @param dpi "dpi used to convert pixels to inches"
#'
#' @return "Width/Height in pixels/inches"
#' @export
#'
get_rendered_plot_height <- function(output_id, session, return_inches = F, dpi=70){
  index <- paste0("output_", session$ns(output_id), "_height")
  height <- session$clientData[[index]]
  
  if (return_inches) return(height/dpi)
  else return(height)
}



# protein domains and transcripts -----------------------------------------
prot_db <- readRDS(file = system.file('extdata', 'protein_domains.RDs', package = 'maftools'))

gene_name_to_refseq_ids <- function(gene_name, return_protein_ids=FALSE){
  utilitybelt::assert_that(gene_name %in% prot_db$HGNC, msg = "gene not found in protein domains.RDs")
  refseq.ids <- prot_db$refseq.ID[prot_db$HGNC==gene_name] %>% unique()
  protein.ids <- prot_db$protein.ID[prot_db$HGNC==gene_name] %>% unique()
  
  if(return_protein_ids) return(protein.ids)
  else return(refseq.ids)
}

gene_name_to_transcript_table <- function(gene_name, longest_first = T){
  utilitybelt::assert_that(gene_name %in% prot_db$HGNC, msg = "gene not found in protein domains.RDs")
  dt <- prot_db %>% 
    dplyr::filter(HGNC == gene_name) %>% 
    dplyr::group_by(refseq.ID) %>% 
    dplyr::summarise(HGNC=unique(HGNC), protein.ID = unique(protein.ID), aa.length = unique(aa.length)) %>%
    dplyr::select(HGNC, everything())
  
  if(longest_first) dt %>% dplyr::arrange(dplyr::desc(aa.length)) %>% return()
  else dt %>% dplyr::arrange(aa.length) %>% return()
  
}

#Removes .gz, .maf or .maf.gz extensions from a string.
filename_remove_extension_maf <- function(filename){
  filename %>% sub(pattern = "\\.gz$", replacement = "", x = .) %>% sub(pattern = "\\.maf$", replacement = "", x = .) %>% return()
}

#Input a maf object. Returns table containing ALL MAF data. Nonsynonymous AND synonymous
maf_get_all_data <- function(maf, include_silent_mutations=T) {
  if(include_silent_mutations)
    data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE) %>% return()
  else
    return(maf@data)
}

