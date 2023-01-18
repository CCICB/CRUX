test_that("maftools_samples_with_mutated_gene correctly lists tsbs", {
  
  acc_maf=maftools::tcgaLoad("ACC", "Firehose")
  
  MUC16_mutated_samples <- acc_maf %>%
    maftools::subsetMaf(genes = "MUC16") %>%
    maftools::getSampleSummary() %>% 
    dplyr::pull(Tumor_Sample_Barcode) %>% sort() %>% 
    as.character()
  
  expect_equivalent(
    maftools_samples_with_mutated_gene(acc_maf, gene = "MUC16"),
    MUC16_mutated_samples
    )
  
  expect_equal(
    maftools_samples_with_mutated_gene(acc_maf, gene = "ASdlkasdjlasjdlkasdjalkl"),
    character(0)
  )
})
