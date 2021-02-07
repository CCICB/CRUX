test_that("Creation of maf_dataset_wrapper from tcga dataset is successful", {
  expect_s3_class(tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), "ACC"), "maf_dataset_wrapper")
  expect_equal(tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(),"ACC")$local_path_to_data, system.file("extdata/Firehose/ACC.RDs", package = "TCGAmutations"))
  expect_true(file.exists(tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(),"ACC")$local_path_to_data))
  expect_equal(tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(),"ACC")$name_of_data_source, "TCGA")
  
  expect_error(tcga_dataset_to_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), "ASKDJLAWDLDJW"))
})

test_that("Creation of maf_dataset_wrapper from tcga dataset AND deposition into the datapool is successful", {
  expect_s3_class(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = new_maf_data_pool()), class = "maf_data_pool")
  expect_length(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ACC", maf_data_pool = new_maf_data_pool()), 1)
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ASDASDAWD", maf_data_pool = new_maf_data_pool()))
  expect_error(tcga_dataset_to_data_pool(tcga_study_abbreviation = "ASDASDAWD", maf_data_pool = "Hello"))
})

test_that("Wrapping all TCGA datasets and adding to data pool works", {

  #Throws errors when expected
  expect_error(
    tcga_datasets_to_data_pool(maf_data_pool = new_maf_data_pool(), source = "ADWAW")
  )
  
  expect_error(
    tcga_datasets_to_data_pool(maf_data_pool = new_maf_data_pool(), source = "Firehose"), NA
    )
  
  expect_error(
    tcga_datasets_to_data_pool(maf_data_pool = new_maf_data_pool(), source = "MC3"), NA
  )
  
  #Testing Output
  maf_data_pool_firehose = tcga_datasets_to_data_pool(maf_data_pool = new_maf_data_pool(), source = "Firehose") 
  maf_data_pool_MC3 = tcga_datasets_to_data_pool(maf_data_pool = new_maf_data_pool(), source = "MC3") 
  
  #Class mmakes sense
  expect_s3_class(maf_data_pool_firehose, "maf_data_pool")
  expect_s3_class(maf_data_pool_MC3, "maf_data_pool")
  
  #All datasets get added
  expect_gt(length(maf_data_pool_firehose), 31) #There are 34 TCGA datasets BUT 2 are only available from one source (Firehose/MC3). We'll use greater than so if more get added this won't break
  expect_gt(length(maf_data_pool_MC3), 31)
  
  
})
