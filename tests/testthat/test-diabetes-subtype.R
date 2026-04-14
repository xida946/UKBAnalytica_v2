test_that("extract_diabetes_subtype_baseline classifies T1/T2 with HbA1c", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    eid = 1:5,
    p53_i0 = as.Date(rep("2010-01-01", 5)),
    p30750_i0 = c(55, 40, 52, 42, NA),
    p20002_i0_a0 = c(1222, 1223, 0, 0, 1220),
    p20008_i0_a0 = c(2000, 2005, NA, NA, 2008)
  )

  out <- suppressMessages(extract_diabetes_subtype_baseline(
    dt = dt,
    sources = c("Self-report"),
    include_hba1c = TRUE
  ))

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 5)
  expect_true(all(c(
    "eid", "T1DM_history", "T2DM_history", "diabetes_hba1c",
    "T2DM_history_enhanced", "Diabetes_history", "diabetes_subtype"
  ) %in% names(out)))

  out1 <- out[eid == 1]
  expect_equal(out1$T1DM_history, 1L)
  expect_equal(out1$diabetes_subtype, "Type1")

  out2 <- out[eid == 2]
  expect_equal(out2$T2DM_history, 1L)
  expect_equal(out2$diabetes_subtype, "Type2")

  out3 <- out[eid == 3]
  expect_equal(out3$T2DM_history, 0L)
  expect_equal(out3$diabetes_hba1c, 1L)
  expect_equal(out3$T2DM_history_enhanced, 1L)
  expect_equal(out3$diabetes_subtype, "Type2")

  out4 <- out[eid == 4]
  expect_equal(out4$Diabetes_history, 0L)
  expect_equal(out4$diabetes_subtype, "No_diabetes")
})


test_that("extract_diabetes_subtype_baseline supports source-only mode", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    eid = 1:3,
    p53_i0 = as.Date(rep("2010-01-01", 3)),
    p30750_i0 = c(55, 52, 42),
    p20002_i0_a0 = c(1222, 0, 1223),
    p20008_i0_a0 = c(2001, NA, 2006)
  )

  out <- suppressMessages(extract_diabetes_subtype_baseline(
    dt = dt,
    sources = c("Self-report"),
    include_hba1c = FALSE
  ))

  expect_true(all(is.na(out$diabetes_hba1c)))
  expect_equal(out[eid == 2]$Diabetes_history, 0L)
  expect_equal(out[eid == 2]$diabetes_subtype, "No_diabetes")
})


test_that("extract_diabetes_subtype_baseline warns when HbA1c column is missing", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(
    eid = 1:2,
    p53_i0 = as.Date(rep("2010-01-01", 2)),
    p20002_i0_a0 = c(1222, 1223),
    p20008_i0_a0 = c(2001, 2006)
  )

  expect_warning(
    out <- suppressMessages(extract_diabetes_subtype_baseline(
      dt = dt,
      sources = c("Self-report"),
      include_hba1c = TRUE
    )),
    "HbA1c column not found"
  )

  expect_equal(out[eid == 1]$diabetes_subtype, "Type1")
  expect_equal(out[eid == 2]$diabetes_subtype, "Type2")
})
