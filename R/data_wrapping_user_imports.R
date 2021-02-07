
#Start with filepaths. End with wrapper.

#' Title
#'
#' @inheritParams maftools::read.maf
#' @param filepath 	tab delimited MAF file. File can also be gz compressed. Required. Alternatively, you can also provide already read MAF file as a dataframe.
#' @inheritParams new_maf_dataset_wrapper
#'
#' @return maf_dataset_wrapper
#' @export 
#'
#' @family DataToWrapper
user_data_filepath_to_class_maf_dataset_wrapper <- function(filepath, clinicalData=NA, maf_data_pool, display_name, short_name, description = "User specified file", data_source = "USER", loaded_data=NA){
  utilitybelt::assert_that(assertthat::is.string(filepath), msg = utilitybelt::fmterror("user_data_filepath_to_class_maf_dataset_wrapper: filepath must be a string (single length character vector), Not a: ", class(filepath)))
  
  utilitybelt::assert_that(file.exists(filepath))
  
  #Assume filepath is pointing to a maf file
  new_maf_dataset_wrapper(
    maf_data_pool = maf_data_pool,
    display_name = display_name, 
    short_name = short_name,
    unique_name = paste0("USER_", filepath), 
    start_status = "ready", 
    data_description = description,
    is_dataset_downloadable = FALSE,
    function_to_load_data  = function(filepath) { maftools::read.maf(maf = filepath, clinicalData = clinicalData, verbose = FALSE) }, 
    name_of_data_source = data_source, 
    local_path_to_data = filepath, 
    clinical_data = clinicalData,
    datatype_of_stored_object = ".mafs",
    loaded_data = loaded_data
  )
  #browser()
}

#' Add User Data to data pool
#'
#' Takes a filepath to user dataset + some medata and creates a maf_dataset_wrapper object, then adds the wrapper to the maf_data_pool
#'
#' @inheritParams tcga_dataset_to_maf_dataset_wrapper
#' @inheritParams maf_data_pool_add_dataset
#' @inheritParams maftools::read.maf
#' 
#' @return returns a data pool object with extra dataset added (maf_data_pool)
#' @export
#' @family DataToWrapper
user_to_dataset_to_data_pool <- function(maf_data_pool, filepath, clinicalData=NA, display_name, short_name, description = "User specified file", data_source = "USER", loaded_data = NA ){
  #browser()
  dataset_wrapper <- user_data_filepath_to_class_maf_dataset_wrapper(filepath = filepath, clinicalData=clinicalData, maf_data_pool = maf_data_pool, display_name=display_name, short_name=short_name, description=description, data_source=data_source, loaded_data = loaded_data)   
  maf_data_pool_add_dataset(maf_dataset_wrapper = dataset_wrapper, maf_data_pool = maf_data_pool)
}