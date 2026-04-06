#' Exclude Early Events for Sensitivity Analysis
#'
#' @description
#' Remove participants who experienced the event within the first `n_years` of
#' follow-up. The returned dataset keeps the same columns and class as the input
#' so it can be passed directly to the standard regression functions.
#'
#' @param data A data.frame or data.table.
#' @param endpoint Character vector of length 2 giving the time and status
#'   columns, e.g. `c("outcome_surv_time", "outcome_status")`.
#' @param n_years Numeric scalar. Events with follow-up time less than or equal
#'   to this value will be excluded.
#' @param copy Logical scalar. If `TRUE` and `data` is a data.table, work on a
#'   copied object before filtering.
#' @param verbose Logical scalar. If `TRUE`, print a short filtering summary.
#'
#' @return An object with the same class and columns as `data`, with filtered
#'   rows removed. A `sensitivity_info` attribute is added for auditability.
#'
#' @examples
#' dt_sens <- sensitivity_exclude_early_events(
#'   data = mtcars,
#'   endpoint = c("wt", "vs"),
#'   n_years = 3
#' )
#'
#' @export
sensitivity_exclude_early_events <- function(data,
                                             endpoint = c("outcome_surv_time", "outcome_status"),
                                             n_years,
                                             copy = TRUE,
                                             verbose = TRUE) {
  if (!isTRUE(copy) && !identical(copy, FALSE)) {
    stop("'copy' must be TRUE or FALSE.")
  }
  if (!isTRUE(verbose) && !identical(verbose, FALSE)) {
    stop("'verbose' must be TRUE or FALSE.")
  }
  if (!is.character(endpoint) || length(endpoint) != 2) {
    stop("'endpoint' must be a character vector of length 2.")
  }

  .validate_sensitivity_inputs(
    data = data,
    required_cols = endpoint,
    n_years = n_years
  )

  dt <- data
  if (isTRUE(copy) && data.table::is.data.table(data)) {
    dt <- data.table::copy(data)
  }

  time_col <- endpoint[[1]]
  status_col <- endpoint[[2]]
  time_vals <- dt[[time_col]]
  status_vals <- dt[[status_col]]

  if (!is.numeric(time_vals)) {
    stop(sprintf("Time column '%s' must be numeric.", time_col))
  }
  if (any(time_vals < 0, na.rm = TRUE)) {
    stop(sprintf("Time column '%s' contains negative values.", time_col))
  }

  observed_status <- unique(status_vals[!is.na(status_vals)])
  if (!all(observed_status %in% c(0, 1))) {
    stop(sprintf(
      "Status column '%s' must use 0/1 coding for censored/event.",
      status_col
    ))
  }

  drop_idx <- !is.na(status_vals) &
    status_vals == 1 &
    !is.na(time_vals) &
    time_vals <= n_years

  if (data.table::is.data.table(dt)) {
    result <- dt[!drop_idx]
  } else {
    result <- dt[!drop_idx, , drop = FALSE]
  }

  info <- list(
    method = "exclude_early_events",
    endpoint = endpoint,
    n_years = n_years,
    n_input = nrow(data),
    n_removed = sum(drop_idx),
    n_output = nrow(result)
  )
  result <- .attach_sensitivity_metadata(result, info)

  if (isTRUE(verbose)) {
    message("[sensitivity_exclude_early_events] Complete")
    message(sprintf(
      "  Input n = %d, removed = %d, output n = %d",
      info$n_input, info$n_removed, info$n_output
    ))
  }

  result
}


#' Exclude Rows with Missing Covariates for Sensitivity Analysis
#'
#' @description
#' Remove participants with missing values in any of the specified covariates.
#' The returned dataset keeps the same columns and class as the input so it can
#' be passed directly to the standard regression functions.
#'
#' @param data A data.frame or data.table.
#' @param covariates Character vector of covariate names to check.
#' @param copy Logical scalar. If `TRUE` and `data` is a data.table, work on a
#'   copied object before filtering.
#' @param verbose Logical scalar. If `TRUE`, print a short filtering summary.
#'
#' @return An object with the same class and columns as `data`, with filtered
#'   rows removed. A `sensitivity_info` attribute is added for auditability.
#'
#' @examples
#' dt_sens <- sensitivity_exclude_missing_covariates(
#'   data = mtcars,
#'   covariates = c("hp", "wt")
#' )
#'
#' @export
sensitivity_exclude_missing_covariates <- function(data,
                                                   covariates,
                                                   copy = TRUE,
                                                   verbose = TRUE) {
  if (!isTRUE(copy) && !identical(copy, FALSE)) {
    stop("'copy' must be TRUE or FALSE.")
  }
  if (!isTRUE(verbose) && !identical(verbose, FALSE)) {
    stop("'verbose' must be TRUE or FALSE.")
  }

  .validate_sensitivity_inputs(
    data = data,
    required_cols = covariates,
    covariates = covariates
  )

  dt <- data
  if (isTRUE(copy) && data.table::is.data.table(data)) {
    dt <- data.table::copy(data)
  }

  if (data.table::is.data.table(dt)) {
    cov_data <- dt[, covariates, with = FALSE]
  } else {
    cov_data <- dt[, covariates, drop = FALSE]
  }

  drop_idx <- !stats::complete.cases(cov_data)

  if (data.table::is.data.table(dt)) {
    result <- dt[!drop_idx]
  } else {
    result <- dt[!drop_idx, , drop = FALSE]
  }

  info <- list(
    method = "exclude_missing_covariates",
    covariates = covariates,
    n_input = nrow(data),
    n_removed = sum(drop_idx),
    n_output = nrow(result)
  )
  result <- .attach_sensitivity_metadata(result, info)

  if (isTRUE(verbose)) {
    message("[sensitivity_exclude_missing_covariates] Complete")
    message(sprintf(
      "  Input n = %d, removed = %d, output n = %d",
      info$n_input, info$n_removed, info$n_output
    ))
  }

  result
}


#' Validate Sensitivity Analysis Inputs
#'
#' @param data The input data.
#' @param required_cols Character vector of required column names.
#' @param n_years Optional numeric scalar for early-event filtering.
#' @param covariates Optional character vector of covariates.
#'
#' @return NULL, invisibly.
#' @keywords internal
#' @noRd
.validate_sensitivity_inputs <- function(data,
                                         required_cols,
                                         n_years = NULL,
                                         covariates = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or data.table.")
  }

  if (!is.character(required_cols) || length(required_cols) == 0 || anyNA(required_cols)) {
    stop("'required_cols' must be a non-empty character vector.")
  }

  missing_vars <- setdiff(required_cols, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "The following variables are not found in data: %s",
      paste(missing_vars, collapse = ", ")
    ))
  }

  if (!is.null(n_years)) {
    if (!is.numeric(n_years) || length(n_years) != 1 || is.na(n_years) ||
        !is.finite(n_years) || n_years <= 0) {
      stop("'n_years' must be a single positive number.")
    }
  }

  if (!is.null(covariates)) {
    if (!is.character(covariates) || length(covariates) == 0 ||
        anyNA(covariates) || any(covariates == "")) {
      stop("'covariates' must be a non-empty character vector.")
    }
  }

  invisible(NULL)
}


#' Attach Sensitivity Metadata
#'
#' @param data The filtered data object.
#' @param info A named list describing the filtering operation.
#'
#' @return The input data with an updated `sensitivity_info` attribute.
#' @keywords internal
#' @noRd
.attach_sensitivity_metadata <- function(data, info) {
  existing <- attr(data, "sensitivity_info", exact = TRUE)

  if (is.null(existing)) {
    attr(data, "sensitivity_info") <- list(info)
    return(data)
  }

  if (is.list(existing)) {
    attr(data, "sensitivity_info") <- c(existing, list(info))
    return(data)
  }

  attr(data, "sensitivity_info") <- list(existing, info)
  data
}
