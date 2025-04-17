#' Default value for ordering of groups
#'
#' @description
#' A default value, `TRUE` or `FALSE` that controls which algorithm to use
#' for calculating groups. See [f_group_by] for more details.
#'
#' @param x A data frame.
#'
#' @returns
#' A logical of length 1, either `TRUE` or `FALSE`.
#'
#' @export
group_by_order_default <- cpp_group_by_order_default
