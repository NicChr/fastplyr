#' A faster `mutate()` with per-group optimisations
#'
#' @inheritParams f_summarise
#' @param .keep Which columns to keep. Options are 'all', 'used', 'unused' and
#' 'none'.
#'
#' @inheritSection f_summarise Details
#'
#' @returns
#' A data frame with added columns.

#' @export
f_mutate <- function(.data, ...,  .by = NULL, .order = group_by_order_default(.data), .keep = "all"){
  mutate_summary(.data, ..., .keep = .keep, .order = .order, .by = {{ .by }})[["data"]]
}
