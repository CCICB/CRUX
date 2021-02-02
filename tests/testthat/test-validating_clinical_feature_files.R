
# Variables Used in Multiple Tests ---------------------------------------------------------------
valid_maf = system.file("test_data/tcga_laml.subsampled.maf.gz", package="shinymaftoolsr")
valid_metadata = system.file("test_data/tcga_laml_annot.tsv", package="shinymaftoolsr")
valid_metadata_extra_tumor_sample_barcode = system.file("test_data/tcga_laml_annot.extra_tumor_sample_barcodes.tsv", package="shinymaftoolsr")
valid_metadata_missing_tumor_sample_barcode = system.file("test_data/tcga_laml_annot.missing_tumor_sample_barcode.tsv", package="shinymaftoolsr") #removed: TCGA-AB-3000
valid_metadata_only_tumor_sample_barcode = system.file("test_data/tcga_laml_annot.only_tumor_sample_barcodes.tsv", package="shinymaftoolsr") 
valid_metadata_duplicated_tumor_sample_barcode = system.file("test_data/tcga_laml_annot.duplicated_tumor_sample_barcode.tsv", package="shinymaftoolsr") #duplicated TCGA-AB-2832 and changed metadata columns to 'TCGA-AB-2832	M55	3655	1'
valid_metadata_csv = system.file("test_data/tcga_laml_annot.csv", package="shinymaftoolsr") 
invalid_metadata_no_tumor_sample_barcode = system.file("test_data/tcga_laml_annot.no_tumor_sample_barcode.tsv", package="shinymaftoolsr")
invalid_maf = system.file("test_data/invalid.maf", package="shinymaftoolsr")

sink(nullfile())
invalid_metadata_no_tumor_sample_barcode_AND_valid_maf=is_valid_clinicalfeaturefile(invalid_metadata_no_tumor_sample_barcode, valid_maf)
invalid_metadata_no_tumor_sample_barcode_AND_valid_maf_ERROR_MESSAGE=is_valid_clinicalfeaturefile_return_error(invalid_metadata_no_tumor_sample_barcode, valid_maf)
sink()

test_that("is_valid_clinicalfeaturefile works", {
  expect_true(is_valid_clinicalfeaturefile(valid_metadata, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata_only_tumor_sample_barcode, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata_duplicated_tumor_sample_barcode, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata_missing_tumor_sample_barcode, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata_extra_tumor_sample_barcode, valid_maf))
  expect_true(is_valid_clinicalfeaturefile(valid_metadata_csv, valid_maf))
  expect_false(invalid_metadata_no_tumor_sample_barcode_AND_valid_maf)
  expect_false(is_valid_clinicalfeaturefile(valid_metadata, invalid_maf))
  
  #Testing MAFs on their own
  expect_true(is_valid_clinicalfeaturefile(NULL, valid_maf))
  expect_false(is_valid_clinicalfeaturefile(NULL, invalid_maf))
  
})

test_that("is_valid_clinicalfeaturefile_return_error works", {
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata_only_tumor_sample_barcode, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata_duplicated_tumor_sample_barcode, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata_missing_tumor_sample_barcode, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata_extra_tumor_sample_barcode, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_identical(is_valid_clinicalfeaturefile_return_error(valid_metadata_csv, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_match(is_valid_clinicalfeaturefile_return_error(valid_metadata, invalid_maf), "Error in data")
  expect_match(invalid_metadata_no_tumor_sample_barcode_AND_valid_maf_ERROR_MESSAGE, "Tumor_Sample_Barcode column not found")
  
  expect_identical(is_valid_clinicalfeaturefile_return_error(NULL, valid_maf), "Clinical Feature File and MAF are both valid")
  expect_match(is_valid_clinicalfeaturefile_return_error(NULL, invalid_maf), "Error in data")
})
