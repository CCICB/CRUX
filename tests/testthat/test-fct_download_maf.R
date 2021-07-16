test_that("download_maf works", {
  maf.path=system.file("test_data/tcga_laml.subsampled.maf.gz",package = "CRUX")
  maf.maf = maftools::read.maf(maf = maf.path, verbose = FALSE)
  output.path=tempfile(pattern = "test_download_maf")
  
  download_maf(maf = maf.maf, file = output.path)
  downloaded_maf_df <- read.table(file = output.path, header = TRUE, sep = "\t")
  unlink(output.path)
  
  expected_output.path <- system.file("test_data/test_download_maf.maf", package = "CRUX")
  expected_output_df <- read.table(file = expected_output.path, header = TRUE, sep = "\t")
  
  expect_identical(expected_output_df, downloaded_maf_df)
  #tools::md5sum(output.path)
  #expect_equivalent(tools::md5sum(output.path), "c674167291908a19599fb1f2c953901c")
  
  
})
