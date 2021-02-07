test_that("Does loading/unloading functionality work", {
  #Generate wrapper
  my_data <- tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), tcga_study_abbreviation = "ACC")

  #Load data
  my_data_loaded <- maf_data_set_wrapper_load_data(my_data)

  #Check on loaded data
  expect_s4_class(my_data_loaded$loaded_data, class = "MAF") 
  expect_equal(my_data_loaded$status, "ready") 
  
  #Unload when finished
  my_data_unloaded <- maf_data_set_wrapper_unload_data(my_data_loaded)
  expect_true(is.na(my_data_unloaded$loaded_data)) 
  expect_equal(my_data_unloaded$status, "not_loaded") 
})
