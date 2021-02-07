test_that("pcawg_dataset_to_maf_dataset_wrapper works", {
  expect_error(
    pcawg_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), "Bone-Cart"),
    NA
    )
  
  expect_s3_class(
    pcawg_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(),"Bone-Cart"),
    "maf_dataset_wrapper"
  )
  
  expect_equal(
    pcawg_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(),"Bone-Cart")$name_of_data_source,
    "PCAWG"
  )
})

test_that("pcawg_dataset_to_data_pool works", {
  
  expect_error(
    pcawg_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), pcawg_study_abbreviation = "Bone-Cart"),
    NA
  )
  
  expect_s3_class(
    pcawg_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), pcawg_study_abbreviation = "Bone-Cart"),
    "maf_data_pool"
  )
  
  expect_length(
    pcawg_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), pcawg_study_abbreviation = "Bone-Cart"),
    1
  )
  
  expect_error(
    pcawg_dataset_to_data_pool(maf_data_pool = "adasdasda", pcawg_study_abbreviation = "Bone-Cart"),
    "maf_data_pool"
    )
  
  expect_error(
    pcawg_dataset_to_data_pool(maf_data_pool = new_maf_data_pool(), pcawg_study_abbreviation = "asdasdasd")
  )
  
  })


test_that("pcawg_datasets_to_data_pool works", {
  
  expect_error(
    pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool()),
    NA
  )
  expect_error(
    pcawg_datasets_to_data_pool(maf_data_pool = "AWLKDWJAKLWD")
  )
  
  expect_s3_class(
    pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool()),
    "maf_data_pool"
  )
  
  expect_length(
    pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool()),
    nrow(pcawg_available())
  )
  
  expect_s3_class(
    pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool())[[1]],
    "maf_dataset_wrapper"
  )
  
  my_data_pool <- new_maf_data_pool()
  mock_data1 = new_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), display_name = "Hi1",short_name = "short_name", unique_name = "unique_name1", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data2 = new_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), display_name = "Hi2",short_name = "short_name", unique_name = "unique_name2", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data3 = new_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), display_name = "Hi3",short_name = "short_name", unique_name = "unique_name3", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data4 = new_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), display_name = "Hi4",short_name = "short_name", unique_name = "unique_name4", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data1)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data2)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data3)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data4)
  
  expect_length(
    pcawg_datasets_to_data_pool(maf_data_pool = my_data_pool),
    4+nrow(pcawg_available())
    )
})