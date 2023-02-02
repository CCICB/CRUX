
#' TCGA to maf_dataset_wrapper
#' 
#' Takes a TCGA cohort abbreviation
#' @inheritParams tcga_datasets_to_data_pool
#' @param tcga_study_abbreviation a TCGA cohort abbreviation (see maftools::tcgaAvailable()) (string)
#' @return functions and values associated with specified TCGA cohort (maf_dataset_wrapper)
#'
#' @family DataToWrapper
#' @examples
#' ACC_maf_dataset_wrapper <- CRUX:::tcga_dataset_to_maf_dataset_wrapper(CRUX:::new_maf_data_pool(), "ACC")
tcga_dataset_to_maf_dataset_wrapper <- function(maf_data_pool, tcga_study_abbreviation, source = "Firehose"){
  #browser()
  tcga_available_df <- maftools::tcgaAvailable()
  
  utilitybeltassertions::assert_non_empty_string(tcga_study_abbreviation, msg = "tcga_study_abbreviation must be a string >0 characters long")
  #browser()
  assertthat::assert_that(tcga_study_abbreviation %in% tcga_available_df[["Study_Abbreviation"]], msg = utilitybeltassertions::fmterror("Failed to find tcga_study_abbreviation [", tcga_study_abbreviation, "] in TCGAmutations database. Check maftools::tcgaAvailable() for a list of valid abbreviations"))
  #browser()
  
  
  row_of_interest <- which(tcga_available_df[["Study_Abbreviation"]] %in% tcga_study_abbreviation)
  full_study_name <- tcga_available_df[["Study_Name"]][row_of_interest]
  number_of_samples <- tcga_available_df[[source]][row_of_interest] %>% 
    sub(pattern = "([[:digit:]]+).*",replacement = "\\1", x = .) %>%
    as.numeric()
  
  #message("SampleNum: ", number_of_samples)
  
  new_maf_dataset_wrapper(
    maf_data_pool = maf_data_pool,
    display_name = full_study_name, 
    short_name = tcga_study_abbreviation,
    unique_name = paste0("TCGA_", tcga_study_abbreviation),  #TODO: add function to ensure this id is UNIQUE in a given datapool (add to base-level new_maf_dataset_wrapper). Make the maf_dataset_wrapper mandatory.
    start_status = "not_loaded", 
    data_description = paste0(source, " data from study: ", full_study_name), 
    is_dataset_downloadable = FALSE,
    function_to_load_data  = function(filepath) { TCGAmutations_load_with_typed_metadata(tcga_study_abbreviation, source) %>% maftools_chrom_23_and_24_to_X_and_Y() },
    name_of_data_source = "TCGA", local_path_to_data = system.file(paste0("extdata/", source, "/", tcga_study_abbreviation, ".RDs"), package = "TCGAmutations"), datatype_of_stored_object = ".RDs", 
    number_of_samples = number_of_samples
  )
}

#' Add TCGA cohort to data pool
#'
#' Takes a TCGA study abbreviation, creates a tcga_dataset_to_maf_dataset_wrapper object for the relevant dataset and adds the wrapper to the maf_data_pool
#'
#' @inheritParams tcga_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#' @inheritParams tcga_datasets_to_data_pool
#'
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#'
#' @family DataToWrapper
#' @examples
#' CRUX:::tcga_dataset_to_data_pool("ACC", CRUX:::new_maf_data_pool())
tcga_dataset_to_data_pool <- function(tcga_study_abbreviation, maf_data_pool, source = "Firehose"){
  dataset_wrapper <- tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = maf_data_pool, tcga_study_abbreviation=tcga_study_abbreviation, source = source)   
  maf_data_pool_add_dataset(maf_dataset_wrapper = dataset_wrapper, maf_data_pool = maf_data_pool)
}



#' Adds all TCGA cohort to data pool
#'
#' Takes a maf_data_poolobject and adds maf_dataset_wrappers for all tcga datasets available
#' 
#'
#' @inheritParams tcga_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#' @param source 'MC3' or 'Firehose'. Source of TCGA data to use. See ?maftools::tcgaLoad for details (string)
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#' 
#' @family DataToWrapper
#' @examples
#' CRUX:::tcga_datasets_to_data_pool(CRUX:::new_maf_data_pool(), source = "Firehose")
#' @export
tcga_datasets_to_data_pool <- function(maf_data_pool, source = "Firehose"){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(source)
  assertthat::assert_that(source %in% c("Firehose", "MC3"), msg = utilitybeltassertions::fmterror("[tcga_datasets_to_data_pool]: '", source, "' is not a valid TCGA source. Valid options are MC3 or Firehose"))
  valid_tcga_abbreviations.v <- maftools::tcgaAvailable()[ !is.na(maftools::tcgaAvailable()[[source]]), "Study_Abbreviation"] %>% unlist()
  
  assertthat::assert_that(length(valid_tcga_abbreviations.v) > 0, msg = utilitybeltassertions::fmterror("No TCGA data was found from the source: [",source,"]. Options valid options are MC3 or Firehose"))
    
  for (tcga_abbreviation in valid_tcga_abbreviations.v) {
    maf_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = tcga_abbreviation, maf_data_pool = maf_data_pool, source=source)
  }
  return(maf_data_pool)
}
