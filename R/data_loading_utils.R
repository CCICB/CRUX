
# Dataset Wrapper Level ---------------------------------------------------
#' Load data into memory
#'
#' Uses the functions in maf_dataset_wrapper to load data into the loaded_data element and update status.
#' 
#' It is only the maf object that is loaded. RNA data stays sitting in a file until it is retrieved via maf_dataset_wrapper_get_rnaseq
#'
#' @param maf_dataset_wrapper the wrapper of the dataset you want to load into memory (maf_dataset_wrapper)
#'
#' @return a copy of the original wrapper with status and loaded data updated. (maf_dataset_wrapper)
#'
#' 
#' @examples
#' #Generate wrapper
#' my_data <- CRUX:::tcga_dataset_to_maf_dataset_wrapper(
#'   maf_data_pool = CRUX:::new_maf_data_pool(), 
#'   tcga_study_abbreviation = "ACC"
#' )
#' 
#' #Load data
#' my_data <- CRUX:::maf_data_set_wrapper_load_data(my_data)
#' 
#' #Access loaded data
#' print(my_data$loaded_data)
#' 
#' #Unload when finished
#' my_data <- CRUX:::maf_data_set_wrapper_unload_data(my_data)
#' 
#' @family data_set_wrapper_loading
maf_data_set_wrapper_load_data <- function(maf_dataset_wrapper){
  #assert_that_class_is_maf_data_pool(maf_data_pool)
  assert_that_class_is_maf_dataset_wrapper(maf_dataset_wrapper)
  updated_maf_dataset_wrapper <- maf_dataset_wrapper
  assertthat::assert_that(maf_dataset_wrapper$status=="not_loaded", msg = paste0("Can only load data if status is: not_loaded. Status is currently: ", maf_dataset_wrapper$status))
  updated_maf_dataset_wrapper$loaded_data <- maf_dataset_wrapper$load_data(maf_dataset_wrapper$local_path_to_data)
  assertthat::assert_that(utilitybeltassertions::class_is(updated_maf_dataset_wrapper$loaded_data, tested_class = "MAF"), msg = paste0("maf_data_set_wrapper_load_data: load function did not load an MAF object"))
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
#'
#' 
#' @inherit maf_data_set_wrapper_load_data examples
#' 
#' @family data_set_wrapper_loading
maf_data_set_wrapper_unload_data <- function(maf_dataset_wrapper){
  assert_that_class_is_maf_dataset_wrapper(maf_dataset_wrapper)
  updated_maf_dataset_wrapper <- maf_dataset_wrapper
  assertthat::assert_that(maf_dataset_wrapper$status=="ready", msg = paste0("Can only unload data unless its already loaded (status == 'ready'). Status is currently:", maf_dataset_wrapper$status))
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
#' 
maf_data_pool_unload_data <- function(maf_data_pool, unique_name){
  index <- maf_data_pool_get_index_from_unique_name(maf_data_pool = maf_data_pool, unique_name = unique_name)
  maf_data_pool[[index]] <-  maf_data_set_wrapper_unload_data(maf_dataset_wrapper = maf_data_pool[[index]])
  return(maf_data_pool) 
}

# RNAseq functionality ----------------------------------------------------

#' Read RNAseq file
#'
#' @param rnaseq_file (string)
#'
#' @return Dataframe containing at least three columns, named "Tumor_Sample_Barcode", "Hugo_Symbol" and "TPM". May optionally include columns named "Fold_Change" and "Transcript" (dataframe)
#'
#'
read_rnaseq_file <- function(rnaseq_file){
  
  assertthat::assert_that(assertthat::is.string(rnaseq_file), msg = "[read_rnaseq_file] expected rnaseq_file to be a string")
  
  #Assert file exists
  assertthat::assert_that(file.exists(rnaseq_file), msg = paste0("Could not find file: ", rnaseq_file))
  
  #Read Data
  rnaseq_df <- data.table::fread(rnaseq_file)
  
  #Check it dataframe looks ok
  assert_rnaseq_df_is_formatted_correctly(rnaseq_df)
  
  #Ok, we can be pretty confident the data looks good
  return(rnaseq_df)
}


#' Check RNAseq Dataframe
#'
#' Runs a bunch of assertions on the supplied rnaseq dataframe to determine whether it is valid.
#' Will error if it is not.
#'
#' @param rnaseq_df Dataframe containing at least three columns, named "Tumor_Sample_Barcode", "Hugo_Symbol" and "TPM". May optionally include columns named "Fold_Change" and "Transcript" (dataframe)
#'
#' @return Nothing, run for its side effects
#' 
assert_rnaseq_df_is_formatted_correctly <- function(rnaseq_df){
  #Assert number of columns is correct
  assertthat::assert_that(ncol(rnaseq_df) >= 3, msg = paste0("RNAseq files requires at between 3 and 5 columns, not [", ncol(rnaseq_df) ,"]. Please include a header line with the following terms: 'Tumor_Sample_Barcode', 'Hugo_Symbol', 'TPM'. Optionally, include 'RefSeq_Transcript' and 'Fold_Change' columns"))
  
  #Assert file has header
  rnaseq_colnames <- colnames(rnaseq_df)
  assertthat::assert_that(! "V1" %in% rnaseq_colnames, msg = "File should have a header containing: 'Tumor_Sample_Barcode', 'Hugo_Symbol', 'TPM', [Optional] 'Transcript', [Optional] 'Fold_Change'")
  
  #Assert names of columns is correct
  valid_colnames <- c("Tumor_Sample_Barcode", "Hugo_Symbol", "TPM", "Transcript", "Fold_Change")
  expected_colnames <- c("Tumor_Sample_Barcode", "Hugo_Symbol", "TPM") 
  
  expected_colnames_in_file <- expected_colnames[expected_colnames %in% rnaseq_colnames]
  expected_colnames_missing_in_file <- expected_colnames[! expected_colnames %in% rnaseq_colnames]
  
  message("Found columns: ", paste0(expected_colnames_in_file, collapse = ", "))
  
  #Assert that all are column names in file are valid
  assertthat::assert_that(all(rnaseq_colnames %in% valid_colnames), msg = paste0("Unexpected columns found: ", paste0(rnaseq_colnames[!rnaseq_colnames %in% valid_colnames], collapse = ",")))
  
  #Assert expected Column names are all in the file
  assertthat::assert_that(all(expected_colnames %in% rnaseq_colnames), msg = paste0("File missing the following columns: ", paste0(expected_colnames_missing_in_file, collapse = ",")))
  
  #Assert there are no duplicate column names:
  assertthat::assert_that(!any(duplicated(rnaseq_colnames)), msg = paste0("Duplicated column names are not allowed. Duplicated columns found: ", paste0(rnaseq_colnames[duplicated(rnaseq_colnames)], collapse = ",")))
  
  #Assert that type of each column is appropriate:
  assertthat::assert_that(class(rnaseq_df[["Hugo_Symbol"]]) == "character", msg = paste0("Hugo_Symbol column should contain characters. Your supplied values were of the class: ", class(rnaseq_df[["Hugo_Symbol"]])))
  assertthat::assert_that(class(rnaseq_df[["TPM"]]) %in% c("numeric", "integer", "double"), msg = paste0("TPM column should only contain numbers. Your supplied values were of the class: ", class(rnaseq_df[["TPM"]])))
  
  if("Fold_Change" %in% rnaseq_colnames)
    assertthat::assert_that(class(rnaseq_df[["Fold_Change"]]) %in% c("numeric", "integer", "double"), paste0(msg = "Fold_Change column should only contain characters. Your supplied values were of the class: ", class(rnaseq_df[["Fold_Change"]]))) 
  
  message("RNAseq data is as expected")
  invisible(NULL)
}


#' Add RNAseq slot to maf_dataset_wrapper
#'
#' Add rnaseq data to maf_dataset_wrapper object
#'
#' @param maf_dataset_wrapper any maf object (maf_dataset_wrapper)
#' @param rnaseq_path path to rnaseq dataset (string) 
#' @return the input maf_dataset_wrapper with rnaseq_path in rnaseq_filepath slot
#'
#'
maf_data_wrapper_add_rnaseq <- function(maf_dataset_wrapper, rnaseq_path){
  #Assertions
  assert_that_class_is_maf_dataset_wrapper(object = maf_dataset_wrapper)
  
  #If rnaseq_path is NA, just set rnaseq filepath to NA, otherwise first make sure the rnaseq file looks right.
  if(!is.na(rnaseq_path))
    rnaseq_df = read_rnaseq_file(rnaseq_path)
  
  maf_dataset_wrapper$rnaseq_filepath <- rnaseq_path
  return(maf_dataset_wrapper)
}

#' Get rnaseq data
#'
#' You probaly want maf_data_wrapper_get_rnaseq_data_for_samples_with_mutation_data instead.
#' 
#' This function takes a maf_dataset_wrapper and retrieves ALL rnaseq data present (inc. for samples that would have been filtered out by subsetting operations).
#'
#' @param maf_dataset_wrapper a maf_dataset wrapper. See ?new_maf_dataset_wrapper for details.
#'
#' @return RNAseq data if present or NULL if no RNA data has been associated with it yet (dataframe)
#'
#'
maf_data_wrapper_get_rnaseq_df <- function(maf_dataset_wrapper){
  if(!maf_data_wrapper_has_rnaseq_data(maf_dataset_wrapper)){
    "No RNA data available ... returning NULL"
    return(NULL)
  }
  else
    read_rnaseq_file(maf_dataset_wrapper$rnaseq_filepath)
}

#' Get Expression Data from maf
#'
#' Similar tomaf_data_wrapper_get_rnaseq_df but only returns expression data for samples that have corresponding mutation data
#'
#' @inheritParams maf_data_wrapper_get_rnaseq_df 
#'
#' @return RNAseq data if present or NULL if no RNA data has been associated with it yet. Will only return expression data for samples that have corresponding mutation data (dataframe)
#'
#' 
#' @examples
#' # Prepare Data
#' \dontrun{
#' rna_path = system.file("example_data/blca_rnaseq.tsv", package = "CRUX")
#' maf_data_wrapper = CRUX:::tcga_dataset_to_maf_dataset_wrapper(CRUX:::new_maf_data_pool(), "BLCA")
#' 
#' # Add RNA data
#' maf_data_wrapper_with_RNA = CRUX:::maf_data_wrapper_add_rnaseq(
#'   maf_data_wrapper, 
#'   rnaseq_path = rna_path
#' ) 
#' 
#' # Retrieve RNA data for samples with mutation data
#' CRUX:::maf_data_wrapper_get_rnaseq_data_for_samples_with_mutation_data(maf_data_wrapper_with_RNA)
#' }
#' 
maf_data_wrapper_get_rnaseq_data_for_samples_with_mutation_data <- function(maf_dataset_wrapper){
  if (!maf_data_wrapper_has_rnaseq_data(maf_dataset_wrapper)) { message("Dataset has no associated expression data. Returning NULL"); return(NULL) }
  
  #Return null if maf isn't loaded 
  if(maf_dataset_wrapper$status != "ready"){
    message("This function requires the maf_dataset_wrapper object is loaded, however status is currently '", maf_dataset_wrapper$status,"'. Returning NULL")
    return(NULL)
  }
  
  maf = maf_dataset_wrapper$loaded_data
  
  rnaseq_df = maf_data_wrapper_get_rnaseq_df(maf_dataset_wrapper)
  
  tsb_dna = maf %>%
    maftools_get_all_data() %>% 
    dplyr::pull(Tumor_Sample_Barcode) %>% 
    unique()
  
  rnaseq_df %>% 
    dplyr::filter(Tumor_Sample_Barcode %in% tsb_dna)
}

#' Maf Dataset has RNAseq data
#'
#' @param maf_dataset_wrapper a maf dataset wrapper. See ?new_maf_dataset_wrapper
#'
#' @return TRUE if RNAseq data is present, False if not (Boolean)
#'
maf_data_wrapper_has_rnaseq_data <- function(maf_dataset_wrapper){
  if(!is.na(maf_dataset_wrapper$rnaseq_filepath) && file.exists(maf_dataset_wrapper$rnaseq_filepath))
    return(TRUE)
  else 
    return(FALSE)
}



#' Add RNAseq -- maf_data_pool version
#' 
#' Add RNA data to a dataset_wrapper within a maf_data_pool.
#' 
#' How this works: We read in the file at rnaseq_path as data frame, check if it looks like we expect.
#' If so, the \strong{filepath} is saved to the maf_dataset_wrapper (NOT THE DATAFRAME!).
#' This is so we don't have to keep the RNA data in memory. When we go to export or visualise, we'll just reload it using \strong{maf_data_wrapper_get_rnaseq_df}, which returns a dataframe (or NULL if none is found). 
#' 
#' 
#' Also note that no cohort subsetting will affect this RNA file, as all we have is the filepath. 
#' This is not a problem, since the functions that export / visualise the data simply load it into memory, then check the clinical datafile to see which samples we need to export. 
#' If they've been subset out of the clinical datafile, the user probably wont want to export RNA data from these samples ... my export function are aware of this.
#' 
#' 
#' Also note, that this file wont be subset
#' 
#' @param maf_data_pool see ?new_maf_data_pool for details (maf_data_pool)
#' @param unique_name Unique name of the maf_data_wrapper. see ?new_maf_data_wrapper for details (string)
#' @param rnaseq_path Path to rnaseq file (string)
#'
#' @return maf_data_pool with updated maf_data_pool_add_rnaseq
#'
maf_data_pool_add_rnaseq <- function(maf_data_pool, unique_name, rnaseq_path){
  maf_data_wrapper_index <- maf_data_pool_get_index_from_unique_name(maf_data_pool, unique_name)
  maf_data_pool[[maf_data_wrapper_index]] <- maf_data_wrapper_add_rnaseq(maf_data_pool[[maf_data_wrapper_index]], rnaseq_path)
  return(maf_data_pool)
}
