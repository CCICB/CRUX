my_data_pool <- new_maf_data_pool()
mock_data1 = new_maf_dataset_wrapper(display_name = "Hi1",short_name = "short_name", unique_name = "unique_name1", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
mock_data2 = new_maf_dataset_wrapper(display_name = "Hi2",short_name = "short_name", unique_name = "unique_name2", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
mock_data3 = new_maf_dataset_wrapper(display_name = "Hi3",short_name = "short_name", unique_name = "unique_name3", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")
mock_data4 = new_maf_dataset_wrapper(display_name = "Hi4",short_name = "short_name", unique_name = "unique_name4", start_status = "not_loaded", data_description = "Some mock data", is_dataset_downloadable = TRUE, function_to_load_data = function(filepath) {maftools::read.maf(system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'))},name_of_data_source = "maftools", local_path_to_data = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools'), datatype_of_stored_object = ".maf")

my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data1)
my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data2)
my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data3)
my_data_pool <- maf_data_pool_add_dataset(maf_data_pool = my_data_pool, mock_data4)

test_that("Conversion to dataframe works", {
  expect_s3_class(maf_data_pool_to_dataframe(maf_data_pool = my_data_pool), "data.frame")
  expect_error(maf_data_pool_to_dataframe(maf_data_pool = "asdasd"))
})

test_that("Conversion to simple dataframe works", {
  expect_error(
    maf_data_pool_to_simple_dataframe(maf_data_pool = my_data_pool),
    NA
  )
  
  expect_s3_class(
    maf_data_pool_to_simple_dataframe(maf_data_pool = my_data_pool),
    "data.frame"
  )
  
  expect_equal(
    nrow(maf_data_pool_to_simple_dataframe(maf_data_pool = my_data_pool)),
    length(my_data_pool)
  )
  
  expect_error(
    maf_data_pool_to_simple_dataframe(maf_data_pool = "aSDASDWDA"),
    "maf_data_pool"
  )
  
  expect_error(
    maf_data_pool_to_simple_dataframe(maf_data_pool = new_maf_data_pool()),
    "empty"
  )
  
})


test_that("Extraction of nonfunctional properties (literally, properties that aren't functions) is successful", {
  mock_data_pool <- list(
    list(
      nonfunctionalparam1 = 1:10,
      functionalparam = function(x) {return(x)},
      nonfunctionalparam2 = "HELLO",
      functionalparam2 = sum,
      nonfunctionalparam3 = NA
         ),
    list(
      nonfunctionalparam1 = 10:20,
      functionalparam = function(x) {return(x+2)},
      nonfunctionalparam2 = "Yo",
      functionalparam2 = sum,
      nonfunctionalparam3 = NA
    )
  ) %>% magrittr::set_attr("class", "maf_data_pool")
  
  mock_data_pool_no_nonfunctional_params <- list(
    list(
      functionalparam = function(x) {return(x)},
      functionalparam2 = sum
    ),
    list(
      functionalparam = function(x) {return(x+2)},
      functionalparam2 = sum
    )
  ) %>% magrittr::set_attr("class", "maf_data_pool")
  
  expect_true(is.character(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = mock_data_pool)))
  expect_equal(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = mock_data_pool), c("nonfunctionalparam1", "nonfunctionalparam2", "nonfunctionalparam3"))
  expect_equal(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = mock_data_pool), c("nonfunctionalparam1", "nonfunctionalparam2", "nonfunctionalparam3"))
  
  expect_error(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = mock_data_pool_no_nonfunctional_params))
  expect_error(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = "ASDASDWdW"))
  expect_error(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = 5))
  expect_error(maf_data_pool_get_all_nonfunction_property_names(maf_data_pool = list()))
})
