test_that("create_disease_definition works correctly", {
  def <- create_disease_definition(
    name = "Test Disease",
    icd10_pattern = "^I71",
    icd9_pattern = "^441",
    sr_codes = c(1065)
  )
  
  expect_type(def, "list")
  expect_equal(def$name, "Test Disease")
  expect_equal(def$icd10_pattern, "^I71")
  expect_equal(def$icd9_pattern, "^441")
  expect_equal(def$sr_codes, c(1065))
})

test_that("get_predefined_diseases returns valid list", {
  diseases <- get_predefined_diseases()
  
  expect_type(diseases, "list")
  expect_true("AA" %in% names(diseases))
  expect_true("Hypertension" %in% names(diseases))
  expect_true("Diabetes" %in% names(diseases))
  expect_true("T1DM" %in% names(diseases))
  expect_true("T2DM" %in% names(diseases))
  expect_true("ESRD" %in% names(diseases))
  expect_true("Motor_Neurone_Disease" %in% names(diseases))
  expect_true("STEMI" %in% names(diseases))
  expect_true("NSTEMI" %in% names(diseases))
  expect_true("Ischaemic_Stroke" %in% names(diseases))
  expect_true("Alzheimers_Disease" %in% names(diseases))
  expect_true("Parkinsonism" %in% names(diseases))
  
  # Check structure of a disease definition
  aa <- diseases$AA
  expect_equal(aa$name, "Aortic Aneurysm")
  expect_equal(aa$icd10_pattern, "^I71")

  expect_equal(diseases$T1DM$icd10_pattern, "^E10")
  expect_equal(diseases$T2DM$icd10_pattern, "^E11")

  # Check algorithm-defined outcome mapping (Category 42)
  expect_equal(diseases$Asthma$algo_date_field, 42014)
  expect_equal(diseases$Asthma$algo_source_field, 42015)
  expect_equal(diseases$COPD$algo_date_field, 42016)
  expect_equal(diseases$COPD$algo_source_field, 42017)
  expect_equal(diseases$MI$algo_date_field, 42000)
  expect_equal(diseases$MI$algo_source_field, 42001)
  expect_equal(diseases$STEMI$algo_date_field, 42002)
  expect_equal(diseases$STEMI$algo_source_field, 42003)
  expect_equal(diseases$NSTEMI$algo_date_field, 42004)
  expect_equal(diseases$NSTEMI$algo_source_field, 42005)
  expect_equal(diseases$Stroke$algo_date_field, 42006)
  expect_equal(diseases$Stroke$algo_source_field, 42007)
  expect_equal(diseases$Ischaemic_Stroke$algo_date_field, 42008)
  expect_equal(diseases$Ischaemic_Stroke$algo_source_field, 42009)
  expect_equal(diseases$Intracerebral_Haemorrhage$algo_date_field, 42010)
  expect_equal(diseases$Intracerebral_Haemorrhage$algo_source_field, 42011)
  expect_equal(diseases$Subarachnoid_Haemorrhage$algo_date_field, 42012)
  expect_equal(diseases$Subarachnoid_Haemorrhage$algo_source_field, 42013)
  expect_equal(diseases$Dementia$algo_date_field, 42018)
  expect_equal(diseases$Dementia$algo_source_field, 42019)
  expect_equal(diseases$Alzheimers_Disease$algo_date_field, 42020)
  expect_equal(diseases$Alzheimers_Disease$algo_source_field, 42021)
  expect_equal(diseases$Vascular_Dementia$algo_date_field, 42022)
  expect_equal(diseases$Vascular_Dementia$algo_source_field, 42023)
  expect_equal(diseases$Frontotemporal_Dementia$algo_date_field, 42024)
  expect_equal(diseases$Frontotemporal_Dementia$algo_source_field, 42025)
  expect_equal(diseases$ESRD$algo_date_field, 42026)
  expect_equal(diseases$ESRD$algo_source_field, 42027)
  expect_equal(diseases$Motor_Neurone_Disease$algo_date_field, 42028)
  expect_equal(diseases$Motor_Neurone_Disease$algo_source_field, 42029)
  expect_equal(diseases$Parkinsonism$algo_date_field, 42030)
  expect_equal(diseases$Parkinsonism$algo_source_field, 42031)
  expect_equal(diseases$Parkinsons$algo_date_field, 42032)
  expect_equal(diseases$Parkinsons$algo_source_field, 42033)
  expect_equal(diseases$Progressive_Supranuclear_Palsy$algo_date_field, 42034)
  expect_equal(diseases$Progressive_Supranuclear_Palsy$algo_source_field, 42035)
  expect_equal(diseases$Multiple_System_Atrophy$algo_date_field, 42036)
  expect_equal(diseases$Multiple_System_Atrophy$algo_source_field, 42037)
})

test_that("combine_disease_definitions merges correctly", {
  diseases <- get_predefined_diseases()
  mace <- combine_disease_definitions(
    diseases$MI, diseases$Stroke,
    name = "MACE"
  )
  
  expect_equal(mace$name, "MACE")
  expect_true(grepl("I21", mace$icd10_pattern))
  expect_true(grepl("I60", mace$icd10_pattern))
})
