
#' TCGA to maf_dataset_wrapper
#' 
#' Takes a TCGA cohort abbreviation
#' 
#' @param tcga_study_abbreviation a TCGA cohort abbreviation (see TCGAmutations::tcga_available()) (string)
#'
#' @return functions and values associated with specified TCGA cohort (maf_dataset_wrapper)
#' @export
#' @family DataToWrapper
#' @examples
#' ACC_maf_dataset_wrapper <- tcga_dataset_to_maf_dataset_wrapper("ACC")
tcga_dataset_to_maf_dataset_wrapper <- function(tcga_study_abbreviation){
  #browser()
  tcga_available_df <- TCGAmutations::tcga_available()
  utilitybelt::assert_non_empty_string(tcga_study_abbreviation, msg = "tcga_study_abbreviation must be a string >0 characters long")
  #browser()
  utilitybelt::assert_that(tcga_study_abbreviation %in% tcga_available_df[["Study_Abbreviation"]], msg = utilitybelt::fmterror("Failed to find tcga_study_abbreviation [", tcga_study_abbreviation, "] in TCGAmutations database. Check TCGAmutations::tcga_available() for a list of valid abbreviations"))
  #browser()
  row_of_interest <- which(tcga_available_df[["Study_Abbreviation"]] %in% tcga_study_abbreviation)
  full_study_name <- tcga_available_df[["Study_Name"]][row_of_interest]
  
  new_maf_dataset_wrapper(
    display_name = full_study_name, 
    short_name = tcga_study_abbreviation,
    unique_name = paste0("TCGA_", tcga_study_abbreviation), 
    start_status = "not_loaded", 
    data_description = paste0("MC3 data from study: ", full_study_name), 
    is_dataset_downloadable = FALSE,
    function_to_load_data  = function(filepath) { TCGAmutations_load_with_typed_metadata(tcga_study_abbreviation, "MC3") }, 
    #function_to_load_data  = function(filepath) { tcgamutations.maf = TCGAmutations::tcga_load(study = tcga_study_abbreviation, source = "MC3"); maftools::read.maf(maf = tcgamutations.maf, clinicalData = type.convert(getClinicalData(tcgamutations.maf))) }, test
    name_of_data_source = "TCGA", local_path_to_data = system.file("extdata/MC3", paste0(tcga_study_abbreviation, ".RDs"), package = "TCGAmutations"), datatype_of_stored_object = ".RDs"
  )
}

#' Add TCGA cohort to data pool
#'
#' Takes a TCGA study abbreviation, creates a tcga_dataset_to_maf_dataset_wrapper object for the relevant dataset and adds the wrapper to the maf_data_pool
#'
#' @inheritParams tcga_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#'
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#' @export
#' @family DataToWrapper
#' @examples
#' tcga_dataset_to_data_pool("ACC", new_maf_data_pool())
tcga_dataset_to_data_pool <- function(tcga_study_abbreviation, maf_data_pool){
  dataset_wrapper <- tcga_dataset_to_maf_dataset_wrapper(tcga_study_abbreviation)   
  maf_data_pool_add_dataset(maf_dataset_wrapper = dataset_wrapper, maf_data_pool = maf_data_pool)
}



#' Adds all TCGA cohort to data pool
#'
#' Takes a maf_data_poolobject and adds maf_dataset_wrappers for all tcga datasets available
#' 
#' (UNTESTED)
#'
#' @inheritParams tcga_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#'
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#' @export
#' @family DataToWrapper
#' @examples
#' tcga_dataset_to_data_pool("ACC", new_maf_data_pool())
tcga_datasets_to_data_pool <- function(maf_data_pool){
  for (tcga_abbreviation in TCGAmutations::tcga_available()[["Study_Abbreviation"]]) {
    if(tcga_abbreviation=="Unknown") next()
    maf_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = tcga_abbreviation, maf_data_pool = maf_data_pool)
  }
  return(maf_data_pool)
}