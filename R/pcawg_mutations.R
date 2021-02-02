# Loading PCAWG datasets ---------------------------------------------------

#' List Available PCAWG datasets
#' 
#' @param file_listing_projects path to a tsv (w/ header) listing all possible projects in the first column
#'
#' @return a dataframe of the available datasets where the first column contains the values you'd need to feed into pcawg_load
#' @export
#'
#' @examples
#' pcawg_available()
pcawg_available <- function(file_listing_projects = system.file("extdata/pcawg_no_intergenic_by_project_code_rds/pcawg_projects.tsv",package="shinymaftools")){
  utilitybelt::assert_that(file.exists(file_listing_projects))
  available_datasets_df <- read.csv(file = file_listing_projects, header = TRUE, sep = "\t") 
  return(available_datasets_df)
}

#' Load a PCAWG dataset into Memory 
#' 
#' Loads specified PCAWG dataset into memory (as a maf object). 
#' 
#' @param dataset_name a valid pcawg dataset name. For a list of available dataset names, run pcawg_available() (string)
#' @param directory_containing_projectfiles the directory containing Rds files storing the maf objects. Each file must have be named 'dataset_name'.rds. (string)
#'
#' @return a maf object
#' @export
#'
#' @examples
#' pcawg_load("Biliary-AdenoCA")
pcawg_load <- function(dataset_name, directory_containing_projectfiles = system.file("extdata/pcawg_no_intergenic_by_project_code_rds",package="shinymaftools")){
  utilitybelt::assert_non_empty_string(dataset_name)
  utilitybelt::assert_that(file.exists(directory_containing_projectfiles))
  utilitybelt::assert_that(dataset_name %in% pcawg_available()[[1]], msg = utilitybelt::fmterror("pcawg_load: Could not find dataset [", dataset_name, "]. To see available datasets, please run pcawg_available()"))
  
  potential_datasets.paths <- dir(directory_containing_projectfiles, full.names = TRUE)
  
  chosen_path <- potential_datasets.paths[ utilitybelt::path_process_vectorized(paths = potential_datasets.paths, extract_basename = TRUE, remove_extension = TRUE) %in% dataset_name]
  
  return(readRDS(chosen_path))
  
  stop("Failed to return dataset")
}
