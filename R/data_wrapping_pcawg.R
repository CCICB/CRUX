#Everything here needs testing

# Create Wrapper ----------------------------------------------------------

#' Create dataset wrapper from pcawg dataset
#'
#' @param pcawg_study_abbreviation Name of PCAWG study. Use pcawg_available to see options (string)
#'
#' @return maf_dataset_wrapper object
#'
#'
#' @examples
#' pcawg_dataset_to_maf_dataset_wrapper("Bone-Cart")
pcawg_dataset_to_maf_dataset_wrapper <- function(maf_data_pool, pcawg_study_abbreviation){
  pcawg_available_df <- PCAWGmutations::pcawg_available()
  utilitybeltassertions::assert_non_empty_string(pcawg_study_abbreviation)
  assertthat::assert_that(pcawg_study_abbreviation %in% pcawg_available_df[[1]], msg = utilitybeltassertions::fmterror("Failed to find pcawg_study_abbreviation [", pcawg_study_abbreviation, "] in pcawgmutations database. Check pcawg_available() for a list of valid abbreviations"))
  
  full_study_name = pcawg_study_abbreviation # Change later
  sample_number = pcawg_available_df %>% 
    dplyr::filter(Abbreviation == pcawg_study_abbreviation) %>%
    dplyr::pull(Samples) %>%
    head(n=1) %>% 
    as.numeric()
  
  new_maf_dataset_wrapper(
    maf_data_pool = maf_data_pool,
    display_name = full_study_name, 
    short_name = full_study_name,
    unique_name = paste0("pcawg_", pcawg_study_abbreviation), 
    start_status = "not_loaded", 
    data_description = paste0("PCAWG dataset: ", full_study_name), 
    is_dataset_downloadable = FALSE,
    function_to_load_data  = function(filepath) { PCAWGmutations::pcawg_load(pcawg_study_abbreviation) }, 
    name_of_data_source = "PCAWG", datatype_of_stored_object = ".RDs", 
    number_of_samples = sample_number
  )
}


#' Add PCAWG dataset to a data pool
#'
#'
#' @param maf_data_pool the datapool to add pcawg study too (maf_data_pool)
#' @inheritParams pcawg_dataset_to_maf_dataset_wrapper
#'
#' @return returns the data pool with the specified dataset added (maf_data_pool)
#'
#'
#' @examples
#' pcawg_dataset_to_data_pool(new_maf_data_pool(), "Bone-Cart")
#' 
pcawg_dataset_to_data_pool <- function(maf_data_pool, pcawg_study_abbreviation){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(pcawg_study_abbreviation)
  dataset_wrapper <- pcawg_dataset_to_maf_dataset_wrapper(maf_data_pool = maf_data_pool, pcawg_study_abbreviation = pcawg_study_abbreviation)
  maf_data_pool <- maf_data_pool_add_dataset(maf_data_pool = maf_data_pool, maf_dataset_wrapper = dataset_wrapper)
  return(maf_data_pool)
}

#' Add all PCAWG datasets to a data pool
#'
#' @inheritParams pcawg_dataset_to_data_pool
#'
#' @return data pool with all pcawg datasets added as maf_dataset_wrappers (maf_data_pool)
#'
#'
#' @examples
#' pcawg_dataset_to_data_pool(new_maf_data_pool)
#' 
pcawg_datasets_to_data_pool <- function(maf_data_pool){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  
  for (dataset_name in PCAWGmutations::pcawg_available()[[1]]){
    maf_data_pool <- pcawg_dataset_to_data_pool(maf_data_pool, dataset_name)
  }
  
  return(maf_data_pool)
}


