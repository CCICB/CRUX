# Collectino of Gistic Functions

#' Tabulate Gistic Gene Files
#' 
#' Parses a gistic path_to_amp_or_del_genes_file
#'
#' @param path_to_amp_or_del_genes_file path to either del_genes.conf_XX.txt OR amp_genes.conf_90.txt. These files can be produced using gistic (string)
#'
#' @return a dataframe containing cytobands, q values, peak locations and which genes are in each peak
#' @export
#'
gistic_tabulate_amp_or_del_genes_file <- function(path_to_amp_or_del_genes_file) {
  assertthat::assert_that(file.exists(path_to_amp_or_del_genes_file), msg = paste0("could not find file:\n", path_to_amp_or_del_genes_file))
  data = read.csv(path_to_amp_genes_file, sep="\t", header = FALSE)  %>%
    dplyr::mutate(V1=ifelse(V1=="", V1[5], V1)) %>%
    t() %>% 
    as.data.frame() 
  
  col_names <- data[1,1:5]
  data <- data[-1, ]
  data %>%
    tidyr::unite(col = "Genes", 5:ncol(.), sep = ";", remove = TRUE) %>%
    magrittr::set_names(col_names) %>%
    dplyr::mutate(`genes in wide peak` = gsub(pattern = ";;|;$", replacement = "", `genes in wide peak`)) %>%
    tidyr::drop_na(1:4)
}
