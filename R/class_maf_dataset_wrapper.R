
# Constructor -------------------------------------------------------------

#' Constructor Objects of Class: \strong{maf_dataset_wrapper}
#'
#' maf_dataset_wrapper objects store the details of each dataset including the functions to download/load the data and its current status 
#' @param maf_data_pool A maf_data_pool object. Used to check supplied 'unique_name' is actually going to be unique. If not, characaters are appended to make it truly unique in the context of the supplied dataframe.  (maf_data_pool)
#' @param display_name Name that the end-user will see (string)
#' @param short_name Abbreviated dataset name (string)
#' @param unique_name Some unique identifier. (string)
#' @param start_status one of: "not_downloaded","not_loaded", "ready" (string)
#' @param data_description a description of the dataset (string)
#' @param is_dataset_downloadable is the dataset downloadable? The alternative is if its packaged in a compressed form with the tool. 
#' @param function_to_download_data a function that when run will download the required data return the path to which it was downloaded if successful. Will return NA if it failed (function that returns string or NA). If is_dataset_downloadable is false, this is ignored (just use default) 
#' @param function_to_load_data a function that takes 'local_path_to_data' as its argument and returns the loaded maf object when complete. 
#' The function needs to take a single argument, but it doesn't actually have to use it. 
#' For example, when loading tcga data using the TCGAmutations package, this function could be \strong{function(filepath){maftools::tcgaLoad()}}. 
#' This function completely ignores the filepath argument but you NEED to include it anyway (function)
#' @param name_of_data_source name of the data source. For Example "USER" or "TCGA" or "PCAWG" (string)
#' @param local_path_to_data a path to data which will be configured based on function_to_download_data. If this option is configured ahead of time (string)
#' @param datatype_of_stored_object type of stored data object. Not used for anything right now, just interesting metadata. examples are \*.Rds or \*.mafs (string)
#' @param derived_from the maf_dataset_wrapper_object from which the new object was derived. If the dataset was obtained directly from an online source, leave as NA (maf_dataset_wrapper or NA)
#' @param loaded_data the loaded R object (MAF or NA)
#' @param number_of_samples the number of samples in the cohort (integer)
#' @param is_dataset_loadable is dataset loadable (flag)
#' @param clinical_data the clinical data of the object 
#' @param rnaseq_filepath the path to the rnaseq data (string)
#' @return an object of type maf_dataset_wrapper (maf_dataset_wrapper)
#' 
#' @section All Properties:
#' 
#' \itemize{
#' \item display_name
#' \item short_name
#' \item unique_name
#' \item status
#' \item data_description
#' \item download_data
#' \item load_data
#' \item name_of_data_source
#' \item local_path_to_data
#' \item datatype_of_stored_object
#' \item loaded_data
#' \item derived_from
#' \item rnaseq_filepath
#' \item number_of_samples
#' }
#' 
#'
#'
new_maf_dataset_wrapper <- function(maf_data_pool, display_name, short_name, unique_name, start_status, data_description, is_dataset_downloadable, function_to_download_data = function() {return(NA)}, is_dataset_loadable = TRUE,function_to_load_data, name_of_data_source="unknown", local_path_to_data="", clinical_data=NA, datatype_of_stored_object="", derived_from = NA, loaded_data=NA, rnaseq_filepath = NA, number_of_samples = NA) {
  #Dev options
  classname = "maf_dataset_wrapper"
  status_options <- c("not_downloaded","not_loaded", "ready")
  
  #Argument Assertions
  if(short_name == "test"){
    #browser() 
  }
  
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(display_name)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  utilitybeltassertions::assert_non_empty_string(short_name)
  utilitybeltassertions::assert_non_empty_string(start_status)
  assertthat::assert_that(assertthat::is.flag(is_dataset_downloadable))
  assertthat::assert_that(assertthat::is.flag(is_dataset_loadable))
  assertthat::assert_that(start_status %in% status_options, msg = paste0("new_maf_dataset_wrapper: start_status [", start_status,"] must be one of [", paste0(status_options, collapse = ", "), "]"))
  utilitybeltassertions::assert_non_empty_string(data_description)
  assertthat::assert_that(is.function(function_to_download_data))
  assertthat::assert_that(is.function(function_to_load_data))
  assertthat::assert_that(utilitybeltassertions::fun_count_arguments(function_to_load_data) == 1, msg = paste0("new_maf_dataset_wrapper: function_to_load_data must take exactly 1 argument (so when running it we can use the filepath of the local file. You do NOT HAVE to use the argument, just make a spot for it :)"))
  utilitybeltassertions::assert_non_empty_string(name_of_data_source)
  assertthat::assert_that(is_na_scalar(derived_from) | class_is_maf_dataset_wrapper(derived_from)) 
  assertthat::assert_that(is_na_scalar(clinical_data) || is.data.frame(clinical_data), msg= paste0("new_maf_dataset_wrapper: Clinical_data must be a dataframe or NULL. It cannot be a: ", class(clinical_data)))
  assertthat::assert_that(identical(loaded_data, NA) || utilitybeltassertions::class_is(loaded_data, "MAF"))
  assertthat::assert_that(is_na_scalar(rnaseq_filepath) | (assertthat::is.string(rnaseq_filepath) && file.exists(rnaseq_filepath)), msg = paste0("new_maf_dataset_wrapper: rnaseq_filepath must be either NA, or a string leading to a valid filepath"))
  assertthat::assert_that(is_na_scalar(number_of_samples) | assertthat::is.number(number_of_samples), msg = paste0("new_maf_dataset_wrapper: number_of_samples must be either NA or a number, not a: ", class(number_of_samples)))
  
  #Make unique_name actually unique
  actually_unique_name <- maf_data_pool_make_name_unique(maf_data_pool = maf_data_pool, unique_name)
  
  #Construct Class
  maf_dataset_wrapper <- list(
    display_name=display_name,
    short_name=short_name,
    unique_name=actually_unique_name,
    status=start_status, 
    data_description = data_description,
    download_data = function_to_download_data,
    load_data = function_to_load_data,
    name_of_data_source=name_of_data_source,
    local_path_to_data=local_path_to_data,
    datatype_of_stored_object=datatype_of_stored_object,
    loaded_data=loaded_data,
    derived_from=derived_from,
    is_dataset_downloadable=is_dataset_downloadable,
    is_dataset_loadable=is_dataset_loadable,
    rnaseq_filepath=rnaseq_filepath,
    number_of_samples=number_of_samples
  )
  
  #Add Class attribute
  attr(maf_dataset_wrapper, "class") <- classname
  
  #Return
  return(maf_dataset_wrapper)
}


# Class Utility Functions -------------------------------------------------------------
#' Check if class is a maf_dataset_wrapper
#' 
#' Check if class is a maf_dataset_wrapper object
#' 
#' @param object some object whose class you want to check 
#'
#' @return TRUE if class of object is maf_dataset_wrapper, FALSE if not. (logical)
#'
#'
#' @examples
#' CRUX:::class_is_maf_dataset_wrapper("Hi")
class_is_maf_dataset_wrapper <- function(object){
  utilitybeltassertions::class_is(object = object, tested_class = "maf_dataset_wrapper")
}



#' Assert that class is maf_data_pool
#'
#' @param object some object whose class you want to assert (anything)
#'
#' @return (invisible) TRUE if assertion succeeds, Throws an error if it doesn't
#'
#' @family class_assertions
assert_that_class_is_maf_dataset_wrapper <- function(object) {
  assertthat::assert_that(
    class_is_maf_dataset_wrapper(object),
    msg = paste0("Object is not a maf_dataset_wrapper Its a: [", class(object),"]")
  )
}

#' Load MAF objects
#'
#' @inheritParams maf_data_pool_load_data
#' 
#' @description
#' This function allows user to pass a maf data pool and a unique name of the dataset of interest.
#' It differs from maf_data_pool_load_data in that maf_data_pool_load_data will throw an error if the status of the loaded object is anything other than "not_loaded".
#' This function will take any valid status and try to get the data loaded.
#' It decides if the data actually needs loading, and if so, loads it.
#' 
#' It is designed to run before you run maf_data_pool_unique_name_to_maf / maf_data_pool_get_data_wrapper_from_unique_name.
#'
#' @return maf_data_pool with the specified datawrapper loaded (maf_data_pool)
#'
maf_data_pool_robust_load <- function(maf_data_pool, unique_name){
  #browser()
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  
  maf_dataset_wrapper = maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool, unique_name)
  
  if(maf_dataset_wrapper[["status"]] == "ready"){
    return(maf_data_pool)
  }
  else if(maf_dataset_wrapper[["status"]] == "not_loaded"){
    new_maf_data_pool = maf_data_pool_load_data(maf_data_pool, unique_name = unique_name)
    return(new_maf_data_pool)
    #maf_data_pool
  }
  else if(maf_dataset_wrapper[["status"]] == "not_downloaded"){
    message("Haven't implemented download functions yet")
    return(maf_data_pool)
  }
  else{
   stop("maf_data_pool_robust_load: Encountered the dataset wrapper: ", unique_name, "Which has a non-standard status: ", maf_dataset_wrapper[["status"]]) 
  }
}

maf_data_pool_robust_load_apply_changes_to_reactval <- function(maf_data_pool, unique_name){
  assertions::assert_reactive(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  new_maf_data_pool <- maf_data_pool_robust_load(maf_data_pool(), unique_name)
  maf_data_pool(new_maf_data_pool)
  return(new_maf_data_pool)
}

#' maf_data_pool_unique_name_to_maf_reactive
#'
#' Loads the relevant MAF if possible, appplies changes to maf_data_pool reactive then returns the loaded maf
#'
#' @inheritParams maf_data_pool_get_data_wrapper_from_unique_name
#' @param maf_data_pool the reactiveVal we want will search for our dataset of interest, and is also the object we apply any changes to. (reactiveVal)
#' @return maf object (maf)
#'
#'
maf_data_pool_unique_name_to_maf_reactive <- function(maf_data_pool, unique_name){
  maf_data_pool_nonreactive <- isolate(maf_data_pool())
  assertions::assert_reactive(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  
  maf_data_pool_robust_load_apply_changes_to_reactval(maf_data_pool, unique_name)
  maf_data_pool(maf_data_pool)
  maf_data_wrapper <- maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool_nonreactive, unique_name)
  return(maf_data_wrapper[["loaded_data"]])
}


#' maf_data_pool_unique_name_to_maf_nonreactive
#'
#' @description
#' Takes a unique name and returns the maf object associated with said entry in the maf_data_pool. 
#' Involves loading the dataset but does NOT actually update the maf_data_pool.
#'
#' @inheritParams maf_data_pool_add_dataset
#' @param unique_name unique name (string)
#'
#' @return return loaded data (maf object, nonreactive)
#'
#'
maf_data_pool_unique_name_to_maf_nonreactive <- function(maf_data_pool, unique_name){
  
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  
  maf_data_wrapper <- maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool, unique_name)
  #maf_data_pool_robust_load_apply_changes_to_reactval(maf_data_pool, unique_name)
  #maf_data_pool(maf_data_pool)
  #browser()
  #maf_data_wrapper <- maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool_nonreactive, unique_name)
  return(maf_data_wrapper[["loaded_data"]])
}


