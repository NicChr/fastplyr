#' Run-length IDs
#'
#' @description
#' `f_consecutive_id()`, an alternative to `dplyr::consecutive_id()`
#' creates an integer vector with values in the range `[1, n + 1)` where
#' `n` is the length of the vector or number of rows of the data frame.
#' It increments the ID when `x[i] != x[i - 1]` thus giving information on
#' when there is a change in value.
#'
#' @param x A vector or data frame.
#'
#' @details
#' 'ALTREP' compact sequences are supported as well.
#'
#' @returns
#' An integer vector of consecutive run IDs in the range `[1, n + 1)`.
#'
#' @export
f_consecutive_id <- cpp_consecutive_id
