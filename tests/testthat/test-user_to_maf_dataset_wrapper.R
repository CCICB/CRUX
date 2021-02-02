test_that("Creation of maf_dataset_wrapper from tcga dataset is successful", {
  valid_maf_filepath <- system.file("test_data/tcga_laml.subsampled.maf.gz" ,package="shinymaftools")
  
  
  maf_dataset_wrapper <- user_data_filepath_to_class_maf_dataset_wrapper(filepath = valid_maf_filepath, display_name = "Subsampled TCGA laml", short_name="LAML_Subsampled")
  expect_s3_class(maf_dataset_wrapper, "maf_dataset_wrapper")
  expect_equal(maf_dataset_wrapper$local_path_to_data, valid_maf_filepath)
  expect_equal(maf_dataset_wrapper$name_of_data_source, "USER")
  
  expect_error(user_data_filepath_to_class_maf_dataset_wrapper(filepath = "Randompath_ASdlkjadlkjaasda", display_name = "Subsampled TCGA laml", short_name="LAML_Subsampled"))
  
  
  valid_maf_filepath <- system.file("test_data/tcga_laml.subsubsampled.maf.gz" ,package="shinymaftools")
  maf_dataset_wrapper <- user_data_filepath_to_class_maf_dataset_wrapper(filepath = valid_maf_filepath, display_name = "Subsampled TCGA laml", short_name="LAML_Subsampled")
})

test_that("Creation of maf_dataset_wrapper from user-defined dataset AND deposition into the datapool is successful", {
  valid_maf_filepath <- system.file("test_data/tcga_laml.subsampled.maf.gz" ,package="shinymaftools")
  
  expect_s3_class(
    user_to_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), filepath = valid_maf_filepath, display_name = "DisplayName", short_name="ShortName", data_source = "Me"), 
    class = "maf_data_pool")
  
  expect_s3_class(
    user_to_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), filepath = valid_maf_filepath, display_name = "DisplayName", short_name="ShortName", data_source = "Me", loaded_data = maftools::read.maf(valid_maf_filepath, verbose = FALSE)), 
    class = "maf_data_pool")
  
  expect_length(
    user_to_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), filepath = valid_maf_filepath, display_name = "DisplayName", short_name="ShortName", data_source = "Me"),
    1)
  expect_error(user_to_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), filepath = "asdasdhjawkdbjawd", display_name = "DisplayName", short_name="LAML_Subsampled", data_source = "Me"))
  expect_error(user_to_dataset_to_data_pool(maf_data_pool = "aDJKWHK", filepath = valid_maf_filepath, display_name = "DisplayName", short_name="LAML_Subsampled", data_source = "Me"))
})



