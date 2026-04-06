test_that("sensitivity_exclude_early_events removes early events and preserves metadata", {
  data <- data.frame(
    id = 1:6,
    time = c(0.5, 1.9, 2.1, 3.0, NA, 4.0),
    status = c(1, 1, 1, 0, 1, NA),
    exposure = c(0.2, 0.4, 0.1, 0.8, 0.7, 0.6),
    age = c(50, 52, 54, 56, 58, 60)
  )

  result <- suppressMessages(
    sensitivity_exclude_early_events(
      data = data,
      endpoint = c("time", "status"),
      n_years = 2
    )
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$id, c(3, 4, 5, 6))
  expect_equal(names(result), names(data))

  info <- attr(result, "sensitivity_info", exact = TRUE)
  expect_true(is.list(info))
  expect_length(info, 1)
  expect_equal(info[[1]]$method, "exclude_early_events")
  expect_equal(info[[1]]$n_removed, 2)
})


test_that("sensitivity_exclude_missing_covariates removes rows with any missing covariate", {
  skip_if_not_installed("data.table")

  data <- data.table::data.table(
    id = 1:5,
    age = c(60, NA, 55, 62, 58),
    sex = c("M", "F", NA, "F", "M"),
    bmi = c(25.1, 27.2, 30.0, 28.4, NA),
    outcome = c(0, 1, 0, 1, 0)
  )

  result <- suppressMessages(
    sensitivity_exclude_missing_covariates(
      data = data,
      covariates = c("age", "sex", "bmi")
    )
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$id, c(1, 4))
  expect_equal(names(result), names(data))

  info <- attr(result, "sensitivity_info", exact = TRUE)
  expect_true(is.list(info))
  expect_length(info, 1)
  expect_equal(info[[1]]$method, "exclude_missing_covariates")
  expect_equal(info[[1]]$n_removed, 3)
})


test_that("sensitivity_exclude_missing_covariates feeds directly into regression", {
  data <- mtcars
  data$vs <- as.integer(data$vs)
  data$age_like <- seq_len(nrow(data))
  data$sex_like <- rep(c(0, 1), length.out = nrow(data))

  data$age_like[c(1, 5)] <- NA
  data$sex_like[c(2)] <- NA

  filtered <- suppressMessages(
    sensitivity_exclude_missing_covariates(
      data = data,
      covariates = c("age_like", "sex_like")
    )
  )

  expect_false(anyNA(filtered[, c("age_like", "sex_like")]))

  result_lm <- runmulti_lm(
    data = filtered,
    main_var = c("wt"),
    covariates = c("age_like", "sex_like"),
    outcome = "mpg"
  )

  result_logit <- runmulti_logit(
    data = filtered,
    main_var = c("wt"),
    covariates = c("age_like", "sex_like"),
    outcome = "vs"
  )

  expect_s3_class(result_lm, "data.frame")
  expect_s3_class(result_logit, "data.frame")
  expect_equal(result_lm$variable, "wt")
  expect_equal(result_logit$variable, "wt")
})


test_that("sensitivity functions can be chained before Cox regression", {
  skip_if_not_installed("survival")

  data <- survival::lung
  data <- data[complete.cases(data[, c("time", "status", "age", "sex", "ph.ecog")]), ]
  data$status <- as.integer(data$status == 2)
  data$time <- data$time / 365.25
  data$ph.ecog[1] <- NA

  filtered <- suppressMessages(
    sensitivity_exclude_early_events(
      data = data,
      endpoint = c("time", "status"),
      n_years = 0.25
    )
  )
  filtered <- suppressMessages(
    sensitivity_exclude_missing_covariates(
      data = filtered,
      covariates = c("age", "sex", "ph.ecog")
    )
  )

  info <- attr(filtered, "sensitivity_info", exact = TRUE)
  expect_true(is.list(info))
  expect_length(info, 2)

  result <- runmulti_cox(
    data = filtered,
    main_var = c("age"),
    covariates = c("sex", "ph.ecog"),
    endpoint = c("time", "status")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$variable, "age")
})


test_that("sensitivity functions validate invalid inputs", {
  data <- mtcars

  expect_error(
    sensitivity_exclude_early_events(
      data = data,
      endpoint = c("wt", "vs"),
      n_years = 0
    ),
    "single positive number"
  )

  expect_error(
    sensitivity_exclude_missing_covariates(
      data = data,
      covariates = character(0)
    ),
    "non-empty character vector"
  )
})
