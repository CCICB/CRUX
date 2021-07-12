#' ZERO dataset to maf_dataset_wrapper
#' 
#' This function takes a ZERO childhood cancer 'cancer type' abbreviation and returns the relevant data in the form of a 'maf_dataset_wrapper'.
#' 
#' This function is powered by ZEROmutationsCCI, which pulls data from the rdrive. The problem with this, is it means that
#' \strong{maf_dataset_wrapper$load_data will ERROR if Rdrive isn't mounted}
#' 
#' @inheritParams tcga_datasets_to_data_pool
#' @param zero_study_abbreviation a ZER cohort abbreviation (see ZEROmutations::zero_available()) (string)
#' @return functions and values associated with specified ZERO cohort (maf_dataset_wrapper)
#'
#' @family DataToWrapper
zero_dataset_to_maf_dataset_wrapper <- function(maf_data_pool, zero_study_abbreviation){
  
  zero_available_df <- ZEROmutationsCCI::zero_available()
  utilitybelt::assert_non_empty_string(zero_study_abbreviation, msg = "zero_study_abbreviation must be a string and >0 characters long")
  
  assertthat::assert_that(zero_study_abbreviation %in% zero_available_df[[1]], msg = utilitybelt::fmterror("Failed to find zero_study_abbreviation [", zero_study_abbreviation, "] in ZEROmutations database. Check ZEROmutations::zero_available() for a list of valid abbreviations"))
  
  row_of_interest <- which(zero_available_df[[1]] %in% zero_study_abbreviation)
  full_study_name <- zero_available_df[[2]][row_of_interest]
  
  sample_number = zero_available_df[[3]][row_of_interest] %>%
    head(n=1) %>% 
    as.numeric()
  
  new_maf_dataset_wrapper(
    maf_data_pool = maf_data_pool,
    display_name = full_study_name, 
    short_name = zero_study_abbreviation,
    unique_name = paste0("ZERO_", zero_study_abbreviation),  #TODO: add function to ensure this id is UNIQUE in a given datapool (add to base-level new_maf_dataset_wrapper). Make the maf_dataset_wrapper mandatory.
    start_status = "not_loaded", 
    data_description = paste0(full_study_name, " data from the ZERO program"), 
    is_dataset_downloadable = FALSE,
    function_to_load_data  = function(filepath) { 
      if(!ZEROmutationsCCI::is_data_available()) 
        return(validate("To access ZERO data, you must be connected to the Rdrive. Please connect and mount the Rdrive, then try again. If you are from the CCI and using Windows, try installing CRUX on your VDI located at `Rdsportal.ccia.org.au.` If the problem persists, email selkamand@ccia.org.au")); 
      
      ZEROmutationsCCI::zero_load(zero_study_abbreviation) %>% maftools_chrom_23_and_24_to_X_and_Y() %>% maftools_fix_clinical_data_types()
      },
    name_of_data_source = "ZERO", datatype_of_stored_object = ".RDs",
    number_of_samples = sample_number
  )
}

#' Add ZERO cohort to data pool
#'
#' Takes a ZERO study abbreviation, creates a zero_dataset_to_maf_dataset_wrapper object for the relevant dataset and adds the wrapper to the maf_data_pool
#'
#' @inheritParams zero_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#'
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#'
#' @family DataToWrapper
zero_dataset_to_data_pool <- function(zero_study_abbreviation, maf_data_pool){
  dataset_wrapper <- zero_dataset_to_maf_dataset_wrapper(maf_data_pool = maf_data_pool, zero_study_abbreviation=zero_study_abbreviation)   
  maf_data_pool_add_dataset(maf_dataset_wrapper = dataset_wrapper, maf_data_pool = maf_data_pool)
}


#' Adds all ZERO cohort to data pool
#'
#' Takes a maf_data_poolobject and adds maf_dataset_wrappers for all ZERO datasets available
#' 
#' @inheritParams zero_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#' 
#' @family DataToWrapper
#' @examples
#' zero_datasets_to_data_pool(new_maf_data_pool())
zero_datasets_to_data_pool <- function(maf_data_pool){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  valid_zero_abbreviations.v <- ZEROmutationsCCI::zero_available()[[1]]
  
  for (zero_abbreviation in valid_zero_abbreviations.v) {
    maf_data_pool <- zero_dataset_to_data_pool(zero_study_abbreviation = zero_abbreviation, maf_data_pool = maf_data_pool)
  }
  return(maf_data_pool)
}
