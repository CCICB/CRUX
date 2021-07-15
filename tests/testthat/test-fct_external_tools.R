test_that("external_tools_convert_maf_to_bbglab", {
  maf = maftools::read.maf(system.file(package = "CRUX","/test_data/external_data_conversion_input.maf.gz"), verbose = FALSE)
  maf_dataset_wrapper = new_maf_dataset_wrapper(maf_data_pool = new_maf_data_pool(), display_name = "Hi", is_dataset_downloadable = FALSE,function_to_load_data = function(file){},  short_name = "Hello", unique_name = "bob", start_status = "not_loaded", data_description = "adasda", loaded_data = maf)
  cgi_output.path = tempfile()
  external_tools_convert_maf_to_bbglab(maf_dataset_wrapper =  maf_dataset_wrapper, filepath = cgi_output.path)
  expect_equivalent(
    tools::md5sum(cgi_output.path),
    expected = "c05ce92686cf7d5d81dad613bb489d75"
    )
  
  unlink(cgi_output.path)
  
})

test_that("external_tool_metadata dataset is up to date", {
  #if this fails try running external_tools_update_builtin_dataset()
  expect_equal(external_tools_load_all_tools(), external_tool_metadata)
})
