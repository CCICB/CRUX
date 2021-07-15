
# Constructor -------------------------------------------------------------


#The new_maf_data_pool object is a list of data_objects each elements being a list
#' Constructor Objects of Class: \strong{maf_data_pool}
#'
#' The maf_data_pool class is simply a list of maf_dataset_wrapper objects.
#'
#' @return object of class maf_data_pool (maf_data_pool)
#'
#'
#' @examples
#' maf_data_pool <- new_maf_data_pool()
new_maf_data_pool <- function() {
  #Dev options
  classname="maf_data_pool"
  
  #Construct Class
  new_maf_data_pool <- list()
  
  #Add Class attribute
  attr(new_maf_data_pool, "class") <- classname
  
  #Return
  return(new_maf_data_pool)
}


# Utilities ---------------------------------------------------------------
#' Check if class is a maf_data_pool
#' 
#' Check if class is a maf_data_pool object
#' 
#' @param object some object whose class you want to check 
#'
#' @return TRUE if class of object is maf_data_pool, FALSE if not. (logical)
#'
#'
#' @examples
#' class_is_maf_data_pool("Hi")
#' class_is_maf_data_pool(new_maf_data_pool())
class_is_maf_data_pool <- function(object){
    utilitybeltassertions::class_is(object = object, tested_class = "maf_data_pool")
}


#' Assert that class is maf_data_pool
#'
#' Assert that class is maf_data_pool object
#'
#' @param object some object whose class you want to assert (anything)
#'
#' @return (invisible) TRUE if assertion succeeds, Throws an error if it doesn't
#'
#' @family class_assertions
assert_that_class_is_maf_data_pool <- function(object) {
  
  assertthat::assert_that(
    utilitybeltassertions::class_is(object, tested_class = "maf_data_pool"),
    msg = utilitybeltassertions::fmterror("Object is not a maf_data_pool. Its a: [", class(object),"]")
    )
  
  
}

#' Add to MAF Data Pool
#' 
#' Add a maf_dataset_wrapper object to the maf_data_pool
#'
#' @param maf_dataset_wrapper object to add to the data pool (maf_dataset_wrapper)
#' @param maf_data_pool the data pool to add the dataset wrapper to (maf_data_pool)
#'
#' @return a new maf_data_pool object with the additional objects appended (maf_data_pool)
#'
#'
maf_data_pool_add_dataset <- function(maf_dataset_wrapper, maf_data_pool) {
  assert_that_class_is_maf_dataset_wrapper(maf_dataset_wrapper)
  assert_that_class_is_maf_data_pool(maf_data_pool)
  #browser()
  assertthat::assert_that(maf_data_pool_unique_name_is_available(maf_data_pool = maf_data_pool, unique_name = maf_dataset_wrapper$unique_name), msg = utilitybeltassertions::fmterror("'unique_name' of maf_dataset_wrapper, [", maf_dataset_wrapper$unique_name, "] is already present in maf_data_pool"))
  maf_data_pool[[length(maf_data_pool)+1]] <- maf_dataset_wrapper
  return(maf_data_pool)
}

#' MAF Data Pool to Dataframe
#' 
#' Converts a data pool to a data.frame form.
#' 
#' @param maf_data_pool the data pool to convert to a dataframe (maf_data_pool)
#'
#' @return a data.frame containing all properties of maf_data_pool except for those containing functions (data.frame)
#' 
maf_data_pool_to_dataframe <- function(maf_data_pool){
  #browser()
  assert_that_class_is_maf_data_pool(maf_data_pool)
  
  columns_to_include = maf_data_pool_get_all_nonfunction_property_names(maf_data_pool)
  
  
  #Get rid of other non-vectorisable properties (e.g. dataframs / maf objects)
  columns_to_include = columns_to_include[columns_to_include!="clinical_data"]
  columns_to_include = columns_to_include[columns_to_include!="loaded_data"]
  # columns_to_include = c(
  #   "display_name",
  #   "unique_name",
  #   "status",
  #   "name_of_data_source",
  #   "local_path_to_data",
  #   "datatype_of_stored_object",
  #   "loaded_data",
  #   "derived_from"
  # )
  
  #browser()
  purrr::map_dfr(maf_data_pool, function(maf_data_wrapper){
    list(
      maf_data_wrapper[columns_to_include]
      )
    })
}

maf_data_pool_to_simple_dataframe <- function(maf_data_pool){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  full_data_pool_df <- maf_data_pool_to_dataframe(maf_data_pool = maf_data_pool)
  
  # columns_to_include = c(
  #   "display_name",
  #   "unique_name",
  #   "status",
  #   "name_of_data_source",
  #   "local_path_to_data",
  #   "datatype_of_stored_object",
  #   "loaded_data",
  #   "derived_from"
  # )
  
  columns_to_include = c(
    "display_name",
    "short_name",
    #"unique_name",
    "name_of_data_source",
    "status",
    "local_path_to_data"
  )
  
  new_column_names = c(
    "Name",
    "Abbreviation",
    #"unique_name",
    "Source",
    "Status",
    "Filepath"
  )
  
  assertthat::assert_that(length(columns_to_include) == length(new_column_names))
  
  columns_not_in_full_data_pool_df.v<- columns_to_include[!columns_to_include %in% names(full_data_pool_df)]
  assertthat::assert_that(length(columns_not_in_full_data_pool_df.v) == 0, msg = utilitybeltassertions::fmterror("maf_data_pool_to_simple_dataframe: columns [", paste0(columns_not_in_full_data_pool_df.v, collapse = ", "), "] were not found in full_data_pool_df"))
  
  simplified_data_pool_df <- full_data_pool_df[columns_to_include]
  
  colnames(simplified_data_pool_df) <- new_column_names
  return(simplified_data_pool_df)
}

#' Get Colnames for maf_data_pool to data.frame conversion
#'
#' Gets the names of all maf_dataframe_wrapper properties that don't hold functions. 
#' This information is useful when converting to a data.frame which can't hold functions
#' 
#' This function works by only looking at the first element in the list, which means if one day we change the constructor such that not all wrappers have the same set of properties, we may see unexpected behaviour.
#' Since it is extremely unlikely this will change, I won't make this more robust for now. If I am wrong ... sorry future me :(
#'
#' @param maf_data_pool data pool of interest (maf_data_pool)
#'
#' @return a vector of list_within_list 'properties' that are not functions (character)
#'
maf_data_pool_get_all_nonfunction_property_names <- function(maf_data_pool){
  #Assertions
  assert_that_class_is_maf_data_pool(maf_data_pool)
  assertthat::assert_that(length(maf_data_pool) > 0, msg = utilitybeltassertions::fmterror("maf_data_pool_get_all_nonfunction_property_names: cannot identify nonfunctional columns if maf_data_pool is empty!"))
  
  #Main
  are_functions = purrr::map_lgl(maf_data_pool[[1]], is.function)
  nonfunctional_parameters = names(are_functions)[!are_functions]
  
  #More Assertions
  assertthat::assert_that(length(nonfunctional_parameters) > 0, msg = utilitybeltassertions::fmterror("maf_data_pool_get_all_nonfunction_property_names: could not find any properties that weren't functions"))
  
  #Return
  return(nonfunctional_parameters)
}


# Add or Remove Datasets --------------------------------------------------
#' Get Unique Names from Maf Data Pool
#'
#' Extract 'unique_name' from each maf_dataset_wrapper within a maf_data_pool.
#' \strong{NOTE: } 'unique_name' is a property of maf_dataset_wrapper, however since these wrappers are created independently from each other (no check for uniqueness at the time of creation) they may not actually be unique. 
#' Please check this using maf_data_pool_validate_unique_names
#'
#' @param maf_data_pool A data pool to mine unique_names from (maf_data_pool) 
#' @family maf_data_pool_utils
#' @return a vector listing the 'unique_name' of each maf_dataset_wrapper in the data pool (character)
#'
#'
maf_data_pool_get_unique_names <- function(maf_data_pool) {
  assert_that_class_is_maf_data_pool(maf_data_pool)
  
  if (length(maf_data_pool)==0) return(character(0))
  
  unique_names <- utilitybeltlists::list_of_lists_retrieve_second_level_property(
      list_of_lists = maf_data_pool, 
      name_of_property = "unique_name", 
      function_for_sublist_assertion = class_is_maf_dataset_wrapper
    )
  
  assertthat::assert_that(length(unique_names) == length(maf_data_pool), msg= "Output of ")
  return(unique_names)
}


#' Check availability of a name in the data pool
#'
#' Checks if a particular 'unique_name' is already being used by an object in the some data pool.
#'
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name some string you want to check is not currently used as the 'unique_name' of any object in the datapool (string). 
#'
#' @return TRUE/FALSE depending on whether the unique_name is available (logical)
#'
#'
maf_data_pool_unique_name_is_available <- function(maf_data_pool, unique_name){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  uniquenames <- maf_data_pool_get_unique_names(maf_data_pool)
  is_available = !(unique_name %in% uniquenames)
  return(is_available)
}


#' Make a name unique (within datapool)
#'
#' Checks if a name is unique in a given data pool. 
#' If not, the function will append 15 letters randomly to the name until it finds a unique name, or surpasses \code{max_number_of_attempts}. 
#' Will throw an error if it can't find a unique name
#'
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name some string you want to check is not currently used as the 'unique_name' of any object in the datapool (string). 
#' @param max_number_of_attempts max number of attempts (whole number) 
#' 
#' @return a unique name (string)
#'
maf_data_pool_make_name_unique <- function(maf_data_pool, name, max_number_of_attempts=50){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(name)
  utilitybeltassertions::assert_is_whole_number(max_number_of_attempts)
  
  set.seed(Sys.time())
  if(maf_data_pool_unique_name_is_available(maf_data_pool = maf_data_pool, unique_name = name))
    return(name)
  else{
    for (i in 1:max_number_of_attempts) {
      random_suffix <- LETTERS[runif(n = 15)]
      potentiall_unique_name = paste0(name, "_", random_suffix)
      if(maf_data_pool_unique_name_is_available(maf_data_pool = maf_data_pool, unique_name = potentiall_unique_name))
        return(potentiall_unique_name)
    }
  }
  
  message(utilitybeltassertions::fmterror("[maf_data_pool_make_name_unique] Attempted to create a unique name ",max_number_of_attempts, " times and failed. Make sure max_number_of_attempts is large enough (> 10). If its already large, this error means have an INSANE number of datasets present (unlikely), or somethings wrong with how I'm setting the seed"))
}




#' Retrieve dataset wrapper from data pool using unique_name
#' 
#' Will throw error if unique_name is not found
#' 
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name unique_name of the maf_dataset_wrapper you're interested in(string). 
#'
#' @return index of the specified maf_dataset_wrapper within the maf_data_pool
#' 
maf_data_pool_get_index_from_unique_name <- function(maf_data_pool, unique_name){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  index_of_matching_entry = which(maf_data_pool_get_unique_names(maf_data_pool = maf_data_pool) == unique_name)
  assertthat::assert_that(length(index_of_matching_entry) != 0, msg = utilitybeltassertions::fmterror("maf_data_pool_get_data_wrapper_from_unique_name: Searching [", substitute(maf_data_pool), "] for elements with the unique_name: ['", unique_name, "'], yielded no results"))
  return(index_of_matching_entry)
}

#' Retrieve dataset wrapper from data pool using unique_name
#' 
#' Will throw error if unique_name is not found
#' 
#' @param maf_data_pool The data pool of interest  (maf_data_pool) 
#' @param unique_name unique_name of the maf_dataset_wrapper fetch (string). 
#'
#' @return specified maf_dataset_wrapper
#' 
maf_data_pool_get_data_wrapper_from_unique_name <- function(maf_data_pool, unique_name){
  assert_that_class_is_maf_data_pool(maf_data_pool)
  utilitybeltassertions::assert_non_empty_string(unique_name)
  index_of_matching_entry = which(maf_data_pool_get_unique_names(maf_data_pool = maf_data_pool) == unique_name)
  assertthat::assert_that(length(index_of_matching_entry) != 0, msg = utilitybeltassertions::fmterror("maf_data_pool_get_data_wrapper_from_unique_name: Searching [", substitute(maf_data_pool), "] for elements with the unique_name: ['", unique_name, "'], yielded no results"))
  return(maf_data_pool[[index_of_matching_entry]])
}




