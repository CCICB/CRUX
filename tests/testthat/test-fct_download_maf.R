test_that("download_maf works", {
  maf.path=system.file("test_data/tcga_laml.subsampled.maf.gz",package = "CRUX")
  maf.maf = maftools::read.maf(maf = maf.path, verbose = FALSE)
  output.path=tempfile(pattern = "test_download_maf")
  
  download_maf(maf = maf.maf, file = output.path)
  tools::md5sum(output.path)
  expect_equivalent(tools::md5sum(output.path), "c674167291908a19599fb1f2c953901c")
  
  unlink(output.path)
})
