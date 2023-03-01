test_that("pcawg_available works", {
  expect_error(
    PCAWGmutations::pcawg_available(),
    NA)
  
  expect_s3_class(
    PCAWGmutations::pcawg_available(),
    "data.frame")
  
  expect_gte(
    ncol(PCAWGmutations::pcawg_available()),
    1)
  
  expect_true(
    ("Lymph-BNHL" %in% PCAWGmutations::pcawg_available()[[1]])
    )
  
  expect_error(
    PCAWGmutations::pcawg_available("ASDKLJWDKLWDALW")
    )
})


test_that("pcawg_load works", {
  expect_error(
    PCAWGmutations::pcawg_load()
    )
  
  expect_error(
    invisible(PCAWGmutations::pcawg_load("Bone-Cart", verbose = FALSE)),
    NA)
  
  expect_s4_class(
    PCAWGmutations::pcawg_load("Bone-Cart", verbose = FALSE),
    "MAF")
  
})