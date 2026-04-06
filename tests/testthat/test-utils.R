test_that("select_incident_by_years classifies incident cases using survival columns", {
  data <- data.frame(
    id = 1:6,
    outcome_surv_time = c(1.2, 4.9, 5.0, 8.1, NA, 3.0),
    outcome_status = c(1, 1, 1, 1, 1, 0),
    exposure = c(0.2, 0.4, 0.1, 0.8, 0.7, 0.6)
  )

  result <- suppressMessages(select_incident_by_years(data))

  expect_s3_class(result, "data.frame")
  expect_equal(result$id, c(1, 2, 3, 4))
  expect_equal(
    result$incident_timing,
    c("within_5_years", "within_5_years", "within_5_years", "after_5_years")
  )
  expect_true(all(result$outcome_status == 1))
})


test_that("select_incident_by_years computes incident time from enrollment and event dates", {
  skip_if_not_installed("data.table")

  data <- data.table::data.table(
    id = 1:4,
    p53_i0 = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01", "2010-01-01")),
    earliest_date = as.Date(c("2012-01-01", "2017-01-01", "2010-01-01", NA))
  )

  result <- suppressMessages(select_incident_by_years(data, n_years = 5))

  expect_s3_class(result, "data.table")
  expect_equal(result$id, c(1, 2))
  expect_true("outcome_surv_time" %in% names(result))
  expect_equal(result$incident_timing, c("within_5_years", "after_5_years"))
  expect_true(all(result$outcome_surv_time > 0))
})


test_that("select_incident_by_years can return split data frames", {
  data <- data.frame(
    id = 1:5,
    outcome_surv_time = c(1.2, 4.9, 5.0, 8.1, 3.0),
    outcome_status = c(1, 1, 1, 1, 0),
    exposure = c(0.2, 0.4, 0.1, 0.8, 0.7)
  )

  result <- suppressMessages(select_incident_by_years(data, output = "split"))

  expect_type(result, "list")
  expect_named(result, c("within_n_years", "after_n_years"))
  expect_s3_class(result$within_n_years, "data.frame")
  expect_s3_class(result$after_n_years, "data.frame")
  expect_equal(result$within_n_years$id, c(1, 2, 3))
  expect_equal(result$after_n_years$id, 4)
  expect_true(all(result$within_n_years$incident_timing == "within_5_years"))
  expect_true(all(result$after_n_years$incident_timing == "after_5_years"))
})


test_that("select_incident_by_years validates invalid inputs", {
  data <- data.frame(
    id = 1:3,
    outcome_surv_time = c(1, 2, 3),
    outcome_status = c(1, 2, 1)
  )

  expect_error(
    select_incident_by_years(data, n_years = 0),
    "single positive number"
  )

  expect_error(
    select_incident_by_years(data),
    "0/1 coding"
  )

  expect_error(
    select_incident_by_years(data.frame(id = 1:3)),
    "Provide 'outcome_surv_time' or both 'p53_i0' and 'earliest_date'"
  )
})
