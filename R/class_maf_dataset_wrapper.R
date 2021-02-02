
# Constructor -------------------------------------------------------------

#' Constructor Objects of Class: \strong{maf_dataset_wrapper}
#'
#' maf_dataset_wrapper objects store the details of each dataset including the functions to download/load the data and its current status 
#'
#' @param display_name Name that the end-user will see (string)
#' @param short_name Abbreviated dataset name (string)
#' @param unique_name Some unique identifier. (string)
#' @param start_status one of: "not_downloaded","not_loaded", "ready" (string)
#' @param data_description a description of the dataset (string)
#' @param is_dataset_downloadable is the dataset downloadable? The alternative is if its packaged in a compressed form with the tool. 
#' @param function_to_download_data a function that when run will download the required data return the path to which it was downloaded if successful. Will return NA if it failed (function that returns string or NA). If is_dataset_downloadable is false, this is ignored (just use default) 
#' @param function_to_load_data a function that takes 'local_path_to_data' as its argument and returns the loaded maf object when complete. 
#' The function needs to take a single argument, but it doesn't actually have to use it. 
#' For example, when loading tcga data using the TCGAmutations package, this function could be \strong{function(filepath){TCGAmutations::tcga_load()}}. 
#' This function completely ignores the filepath argument but you NEED to include it anyway (function)
#' @param name_of_data_source name of the data source. For Example "USER" or "TCGA" or "PCAWG" (string)
#' @param local_path_to_data a path to data which will be configured based on function_to_download_data. If this option is configured ahead of time (string)
#' @param datatype_of_stored_object type of stored data object. Not used for anything right now, just interesting metadata. examples are \*.Rds or \*.mafs (string)
#' @param derived_from the maf_dataset_wrapper_object from which the new object was derived. If the dataset was obtained directly from an online source, leave as NA (maf_dataset_wrapper or NA)
#' @param loaded_data the loaded R object (MAF or NA)
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
#' }
#' 
#' @export
#'
new_maf_dataset_wrapper <- function(display_name, short_name, unique_name, start_status, data_description, is_dataset_downloadable, function_to_download_data = function() {return(NA)}, is_dataset_loadable = TRUE,function_to_load_data, name_of_data_source="unknown", local_path_to_data="", clinical_data=NA, datatype_of_stored_object="", derived_from = NA, loaded_data=NA) {
  #Dev options
  classname = "maf_dataset_wrapper"
  status_options <- c("not_downloaded","not_loaded", "ready")
  
  #Argument Assertions
  if(short_name == "test"){
    #browser() 
  }
  
  #browser()
  utilitybelt::assert_non_empty_string(display_name)
  utilitybelt::assert_non_empty_string(unique_name)
  utilitybelt::assert_non_empty_string(short_name)
  utilitybelt::assert_non_empty_string(start_status)
  utilitybelt::assert_that(assertthat::is.flag(is_dataset_downloadable))
  utilitybelt::assert_that(assertthat::is.flag(is_dataset_loadable))
  utilitybelt::assert_that(start_status %in% status_options, msg = utilitybelt::fmterror("new_maf_dataset_wrapper(): start_status [", start_status,"] must be one of [", paste0(status_options, collapse = ", "), "]"))
  utilitybelt::assert_non_empty_string(data_description)
  utilitybelt::assert_that(is.function(function_to_download_data))
  utilitybelt::assert_that(is.function(function_to_load_data))
  utilitybelt::assert_that(utilitybelt::fun_count_arguments(function_to_load_data) == 1, msg = utilitybelt::fmterror("new_maf_dataset_wrapper: function_to_load_data must take exactly 1 argument (so when running it we can use the filepath of the local file. You do NOT HAVE to use the argument, just make a spot for it :)"))
  utilitybelt::assert_non_empty_string(name_of_data_source)
  utilitybelt::assert_that(is.na(derived_from) | class_is_maf_dataset_wrapper(derived_from)) 
  utilitybelt::assert_that(is.na(clinical_data) || is.data.frame(clinical_data), msg= utilitybelt::fmterror("new_maf_dataset_wrapper: Clinical_data must be a dataframe or NULL. It cannot be a: ", class(clinical_data)))
  utilitybelt::assert_that(identical(loaded_data, NA) || utilitybelt::class_is(loaded_data, "MAF"))
  
  
  #Construct Class
  maf_dataset_wrapper <- list(
    display_name=display_name,
    short_name=short_name,
    unique_name=unique_name,
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
    is_dataset_loadable=is_dataset_loadable
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
#' @export
#'
#' @examples
#' class_is_maf_dataset_wrapper("Hi")
class_is_maf_dataset_wrapper <- function(object){
  utilitybelt::class_is(object = object, tested_class = "maf_dataset_wrapper")
}



#' Assert that class is maf_data_pool
#'
#' @param object some object whose class you want to assert (anything)
#'
#' @return (invisible) TRUE if assertion succeeds, Throws an error if it doesn't
#' @export
#' @family class_assertions
#' @examples
#' assert_that_class_is_maf_data_pool(new_maf_data_pool())
assert_that_class_is_maf_dataset_wrapper <- function(object) {
  utilitybelt::assert_that(
    class_is_maf_dataset_wrapper(object),
    msg = utilitybelt::fmterror("Object is not a maf_dataset_wrapper Its a: [", class(object),"]")
  )
}

#' Load MAF objects
#'
#' @inheritParams maf_data_pool_load_data
#' 
#' @description
#' This function allows user to pass a maf data pool and a unique name of the dataset of interest.
#' I differs from maf_data_pool_load_data in that maf_data_pool_load_data will throw an error if the status of the loaded object is anything other than "not_loaded".
#' This function will take any valid status and try to get the data loaded.
#' It decides if the data actually needs loading, and if so, loads it.
#' It is designed to run before you run maf_data_pool_unique_name_to_maf.
#'
#' @return maf_data_pool with the specified datawrapper loaded (maf_data_pool)
#' @export
maf_data_pool_robust_load <- function(maf_data_pool, unique_name){
  #browser()
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybelt::assert_non_empty_string(unique_name)
  
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
  utilitybeltshiny::assert_reactive(maf_data_pool)
  utilitybelt::assert_non_empty_string(unique_name)
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
#' @export
#'
maf_data_pool_unique_name_to_maf_reactive <- function(maf_data_pool, unique_name){
  maf_data_pool_nonreactive <- isolate(maf_data_pool())
  utilitybeltshiny::assert_reactive(maf_data_pool)
  utilitybelt::assert_non_empty_string(unique_name)
  
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
#' @param maf_data_pool 
#' @param unique_name 
#'
#' @return
#' @export
#'
maf_data_pool_unique_name_to_maf_nonreactive <- function(maf_data_pool, unique_name){
  
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybelt::assert_non_empty_string(unique_name)
  
  maf_data_wrapper <- maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool, unique_name)
  #maf_data_pool_robust_load_apply_changes_to_reactval(maf_data_pool, unique_name)
  #maf_data_pool(maf_data_pool)
  #browser()
  #maf_data_wrapper <- maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = maf_data_pool_nonreactive, unique_name)
  return(maf_data_wrapper[["loaded_data"]])
}

