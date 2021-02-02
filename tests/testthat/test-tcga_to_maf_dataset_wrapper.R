test_that("Creation of maf_dataset_wrapper from tcga dataset is successful", {
  expect_s3_class(tcga_dataset_to_maf_dataset_wrapper("ACC"), "maf_dataset_wrapper")
  expect_equal(tcga_dataset_to_maf_dataset_wrapper("ACC")$local_path_to_data, system.file("extdata/MC3/ACC.RDs", package = "TCGAmutations"))
  expect_true(file.exists(tcga_dataset_to_maf_dataset_wrapper("ACC")$local_path_to_data))
  expect_equal(tcga_dataset_to_maf_dataset_wrapper("ACC")$name_of_data_source, "TCGA")
  
  expect_error(tcga_dataset_to_maf_dataset_wrapper("ASKDJLAWDLDJW"))
})

test_that("Creation of maf_dataset_wrapper from tcga dataset AND deposition into the datapool is successful", {
  expect_s3_class(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = new_maf_data_pool()), class = "maf_data_pool")
  expect_length(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = new_maf_data_pool()), 1)
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ASDASDAWD", maf_data_pool = new_maf_data_pool()))
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ASDASDAWD", maf_data_pool = "Hello"))
})
