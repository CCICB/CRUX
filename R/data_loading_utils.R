
# Dataset Wrapper Level ---------------------------------------------------
#' Load data into memory
#'
#' Uses the functions in maf_dataset_wrapper to load data into the loaded_data element and update status.
#'
#' @param maf_dataset_wrapper the wrapper of the dataset you want to load into memory (maf_dataset_wrapper)
#'
#' @return a copy of the original wrapper with status and loaded data updated. (maf_dataset_wrapper)
#' @export
#' 
#' @examples
#' #Generate wrapper
#' my_data <- tcga_dataset_to_maf_dataset_wrapper(tcga_study_abbreviation = "ACC")
#' 
#' #Load data
#' my_data <- maf_data_set_wrapper_load_data(my_data)
#' 
#' #Access loaded data
#' print(my_data$loaded_data)
#' 
#' #Unload when finished
#' my_data <- maf_data_set_wrapper_unload_data(my_data)
#' @family data_set_wrapper_loading
maf_data_set_wrapper_load_data <- function(maf_dataset_wrapper){
  #assert_that_class_is_maf_data_pool(maf_data_pool)
  assert_that_class_is_maf_dataset_wrapper(maf_dataset_wrapper)
  updated_maf_dataset_wrapper <- maf_dataset_wrapper
  utilitybelt::assert_that(maf_dataset_wrapper$status=="not_loaded", msg = utilitybelt::fmterror("Can only load data if status is: not_loaded. Status is currently: ", maf_dataset_wrapper$status))
  updated_maf_dataset_wrapper$loaded_data <- maf_dataset_wrapper$load_data(maf_dataset_wrapper$local_path_to_data)
  utilitybelt::assert_that(utilitybelt::class_is(updated_maf_dataset_wrapper$loaded_data, tested_class = "MAF"), msg = utilitybelt::fmterror("maf_data_set_wrapper_load_data: load function did not load an MAF object"))
  updated_maf_dataset_wrapper$status <- "ready"
  return(updated_maf_dataset_wrapper)
}


#' Unload data from memory
#'
#' Uses the functions in maf_dataset_wrapper to load data into the loaded_data element and update status.
#'
#' @param maf_dataset_wrapper the wrapper of the dataset you want to unload into memory (maf_dataset_wrapper)
#'
#' @return a copy of the original wrapper with status and loaded data elements updated. Status is changed from "ready" => "not_loaded". loaded_data is changed from maf_object to NA. (maf_dataset_wrapper)
#' @export
#' 
#' @inherit maf_data_set_wrapper_load_data examples
#' 
#' @family data_set_wrapper_loading
maf_data_set_wrapper_unload_data <- function(maf_dataset_wrapper){
  assert_that_class_is_maf_dataset_wrapper(maf_dataset_wrapper)
  updated_maf_dataset_wrapper <- maf_dataset_wrapper
  utilitybelt::assert_that(maf_dataset_wrapper$status=="ready", msg = utilitybelt::fmterror("Can only unload data unless its already loaded (status == 'ready'). Status is currently:", maf_dataset_wrapper$status))
  updated_maf_dataset_wrapper$loaded_data <- NA
  updated_maf_dataset_wrapper$status <- "not_loaded"
  return(updated_maf_dataset_wrapper)
}

# Data Pool Level ---------------------------------------------------

#' Load a dataset wrapper from data pool using unique_name
#' 
#' @description 
#' use maf_data_pool_robust_load instead. It wraps this function
#' Will throw error if unique_name is not found or status is anything other than 'not_loaded'.
#' 
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name unique_name of the maf_dataset_wrapper you want to load (string). 
#'
#' @return maf_data_pool object with the wrapped dataset now loaded (maf_data_pool)
maf_data_pool_load_data <- function(maf_data_pool, unique_name){
  index <- maf_data_pool_get_index_from_unique_name(maf_data_pool = maf_data_pool, unique_name = unique_name)
  #browser()
  maf_data_pool[[index]] <-  maf_data_set_wrapper_load_data(maf_dataset_wrapper = maf_data_pool[[index]])
  #browser()
  return(maf_data_pool) 
}

#' Unload dataset wrapper from data pool using unique_name
#' 
#' Will throw error if unique_name is not found
#' 
#' @inherit maf_data_pool_load_data title description return 
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name unique_name of the maf_dataset_wrapper you want to unload (string). 
#'
#' @return maf_data_pool object with the wrapped dataset now unloaded (maf_data_pool)
#' @export 
maf_data_pool_unload_data <- function(maf_data_pool, unique_name){
  index <- maf_data_pool_get_index_from_unique_name(maf_data_pool = maf_data_pool, unique_name = unique_name)
  maf_data_pool[[index]] <-  maf_data_set_wrapper_unload_data(maf_dataset_wrapper = maf_data_pool[[index]])
  return(maf_data_pool) 
}
