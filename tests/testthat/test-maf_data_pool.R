test_that("We can identify when 'unique_names' are not unique", {
  
  expect_true(maf_data_pool_unique_name_is_available(new_maf_data_pool(), unique_name = "Anything"))
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = new_maf_data_pool()), NA) #Expect Success
  
  maf_data_pool <- new_maf_data_pool()
  maf_data_pool_acc <- tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = maf_data_pool)
  expect_false(maf_data_pool_unique_name_is_available(maf_data_pool_acc, unique_name = "TCGA_ACC"))
  expect_true(maf_data_pool_unique_name_is_available(maf_data_pool_acc, unique_name = "RANDOM_NAME"))
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = maf_data_pool_acc))
  expect_error(maf_data_pool_unique_name_is_available(maf_data_pool_acc, unique_name = ""), "empty")
  
  
})


test_that("We can add a bunch of data to the data pool", {
  my_data_pool <- new_maf_data_pool()
  my_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = my_data_pool)
  my_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = "BLCA", maf_data_pool = my_data_pool)
  my_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = "BRCA", maf_data_pool = my_data_pool)
  my_data_pool <- tcga_dataset_to_data_pool(tcga_study_abbreviation = "DLBC", maf_data_pool = my_data_pool)
  
  expect_length(my_data_pool, 4)
  
  
})

test_that("We can succesfully extract different maf_dataset_wrappers from data pool using unique_names", {
  mock_data1 = new_maf_dataset_wrapper(display_name = "Hi1", short_name = "short_name", unique_name = "unique_name1", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data2 = new_maf_dataset_wrapper(display_name = "Hi2", short_name = "short_name", unique_name = "unique_name2", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data3 = new_maf_dataset_wrapper(display_name = "Hi3", short_name = "short_name", unique_name = "unique_name3", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data4 = new_maf_dataset_wrapper(display_name = "Hi4", short_name = "short_name", unique_name = "unique_name4", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  
  my_data_pool <- new_maf_data_pool()
  my_data_pool <- maf_data_pool_add_dataset(maf_dataset_wrapper = mock_data1, maf_data_pool = my_data_pool)
  my_data_pool <- maf_data_pool_add_dataset(maf_dataset_wrapper = mock_data2, maf_data_pool = my_data_pool)
  my_data_pool <- maf_data_pool_add_dataset(maf_dataset_wrapper = mock_data3, maf_data_pool = my_data_pool)
  my_data_pool <- maf_data_pool_add_dataset(maf_dataset_wrapper = mock_data4, maf_data_pool = my_data_pool)
  
  
  expect_length(my_data_pool, 4)
  expect_equivalent(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "unique_name1"), mock_data1)
  expect_equivalent(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "unique_name3"), mock_data3)
  expect_equivalent(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "unique_name2"), mock_data2)
  expect_equivalent(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool,  unique_name = "unique_name4"), mock_data4)
  expect_error(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "asdkladjklwdk"), "no results")
  
  expect_s3_class(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "unique_name1"), "maf_dataset_wrapper")
  expect_s3_class(maf_data_pool_get_data_wrapper_from_unique_name(maf_data_pool = my_data_pool, unique_name = "unique_name4"), "maf_dataset_wrapper")
})

test_that("We can succesfully load data from unique_name", {
  my_data_pool <- new_maf_data_pool()
  mock_data1 = new_maf_dataset_wrapper(display_name = "Hi1", short_name = "short_name", unique_name = "unique_name1", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data2 = new_maf_dataset_wrapper(display_name = "Hi2", short_name = "short_name", unique_name = "unique_name2", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data3 = new_maf_dataset_wrapper(display_name = "Hi3", short_name = "short_name", unique_name = "unique_name3", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  mock_data4 = new_maf_dataset_wrapper(display_name = "Hi4", short_name = "short_name", unique_name = "unique_name4", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), verbose = FALSE)},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
  
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data1)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data2)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data3)
  my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data4)
  
  my_data_pool_loaded_mock_data1 <- maf_data_pool_load_data(maf_data_pool = my_data_pool, unique_name = "unique_name1")
  my_data_pool_unloaded_mock_data1 <- maf_data_pool_unload_data(maf_data_pool = my_data_pool_loaded_mock_data1, unique_name = "unique_name1")
  
  expect_true(is.na(my_data_pool[[1]]$loaded_data))
  expect_s4_class(my_data_pool_loaded_mock_data1[[1]]$loaded_data, class = "MAF")
  expect_true(is.na(my_data_pool_unloaded_mock_data1[[1]]$loaded_data))
  
  expect_error(maf_data_pool_load_data(maf_data_pool = my_data_pool_loaded_mock_data1, unique_name = "unique_name1"))
  
})
