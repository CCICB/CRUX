# maf_data_pool = pcawg_datasets_to_data_pool(maf_data_pool = new_maf_data_pool())
# 
# test_that("maf_data_pool_too_network_map works", {
#   expect_error(
#     maf_data_pool_too_network_map(maf_data_pool = maf_data_pool), 
#     NA
#   )
#   
#   expect_s3_class(
#     maf_data_pool_too_network_map(maf_data_pool = maf_data_pool), 
#     "sigmajs"
#   )
#   
#   expect_error(
#     maf_data_pool_too_network_map(maf_data_pool = "asdawsad"), 
#     "maf_data_pool"
#   )
#   
#   expect_error(
#     maf_data_pool_too_network_map(maf_data_pool = list(element1=LETTERS[1:5], element1=letters[1:9])), 
#     "maf_data_pool"
#   )
#   
#   expect_error(
#     maf_data_pool_too_network_map(maf_data_pool = new_maf_data_pool()), 
#     "empty"
#   )
#   
# })


