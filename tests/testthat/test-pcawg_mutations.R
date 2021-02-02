test_that("pcawg_available works", {
  expect_error(
    pcawg_available(),
    NA)
  
  expect_s3_class(
    pcawg_available(),
    "data.frame")
  
  expect_gte(
    ncol(pcawg_available()),
    1)
  
  expect_true(
    ("Lymph-BNHL" %in% pcawg_available()[[1]])
    )
  
  expect_error(
    pcawg_available("ASDKLJWDKLWDALW"),
    "exist"
    )
})


test_that("pcawg_load works", {
  expect_error(
    pcawg_load()
    )
  
  expect_error(
    pcawg_load("Bone-Cart"),
    NA)
  
  expect_s4_class(
    pcawg_load("Bone-Cart"),
    "MAF")
  
})