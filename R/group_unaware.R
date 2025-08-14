#' Get list of current group-unaware functions
#'
#' @returns
#' A named list of functions marked as group-unaware in fastplyr.
#'
#' @examples
#' library(fastplyr)
#'
#' fns <- get_group_unaware_fns()
#'
#' names(fns)
#' fns$round
#'
#' @export
get_group_unaware_fns <- cpp_group_unaware_fns
