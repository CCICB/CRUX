test_that("TCGAmutations_load_with_typed_metadata works", {
  #Test 1: No Errors
  expect_error(TCGAmutations_load_with_typed_metadata(study = "GBM", source = "MC3"), NA)
  expect_error(TCGAmutations_load_with_typed_metadata(study = "ACC", source = "MC3"), NA)
  
  #Test 2: Errors where expected
  expect_error(TCGAmutations_load_with_typed_metadata(study = "ALWDWD", source = "MC3"))
  expect_error(TCGAmutations_load_with_typed_metadata(study = "GBM", source = "ASDW"))
  
  #Test 3: Output type is correct
  expect_s4_class(TCGAmutations_load_with_typed_metadata(study = "ACC", source = "MC3"), class = "MAF")
  
  #Test 3: Output is as expected
  gbm_mc3_coltypes_known.v <- c("logical","character","character","character","character","character","integer","integer","integer","character","character","character","character","character","character","character","character","character","character","integer","integer","integer","character","integer","integer","integer","character","character","character","character","character","logical","character","character","character","integer","integer","character","character","character","character","character","integer","character","character")
  names(gbm_mc3_coltypes_known.v) <- c("additional_studies","tumor_tissue_site","histological_type","prior_glioma","gender","vital_status","days_to_birth","days_to_death","days_to_last_followup","race_list","tissue_source_site","patient_id","bcr_patient_uuid","informed_consent_verified","icd_o_3_site","icd_o_3_histology","icd_10","tissue_prospective_collection_indicator","tissue_retrospective_collection_indicator","days_to_initial_pathologic_diagnosis","age_at_initial_pathologic_diagnosis","year_of_initial_pathologic_diagnosis","person_neoplasm_cancer_status","day_of_form_completion","month_of_form_completion","year_of_form_completion","ethnicity","other_dx","history_of_neoadjuvant_treatment","initial_pathologic_diagnosis_method","init_pathology_dx_method_other","anatomic_neoplasm_subdivision","radiation_therapy","postoperative_rx_tx","primary_therapy_outcome_success","karnofsky_performance_score","eastern_cancer_oncology_group","performance_status_scale_timing","has_new_tumor_events_information","has_drugs_information","has_radiations_information","has_follow_ups_information","surv_event","Tumor_Sample_Barcode","sample_type_description")
  maf <- TCGAmutations_load_with_typed_metadata(study = "GBM", source = "MC3")
  gbm_mc3_coltypes_from_function.v <- maftools::getClinicalData(maf) %>% sapply(class)
  expect_equal(gbm_mc3_coltypes_from_function.v, gbm_mc3_coltypes_known.v, ignore_attr = TRUE)
  
})
