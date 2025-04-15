#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#'
#' @param data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .sort `r lifecycle::badge("deprecated")`  Use `.order` instead.
#' @param .order Should the groups be calculated as ordered groups?
#' Setting to `TRUE` may sometimes offer a speed benefit, but usually this
#' is not the case. The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of distinct groups.
#'
#' @export
f_distinct <- function(data, ..., .keep_all = FALSE,
                       .order = FALSE, .sort = deprecated(),
                       .by = NULL, .cols = NULL){

  if (lifecycle::is_present(.sort)) {
    lifecycle::deprecate_warn("1.0.0", "f_distinct(.sort = )", "f_distinct(.order = )")
    .order <- .sort
  }

  if (dots_length(...) == 0 && is.null(.cols)){
    .cols <- names(data)
  }
  group_info <- tidy_GRP(data, ..., .by = {{ .by }}, .cols = .cols,
                         .order = .order)
  data <- GRP_data(group_info)
  if (.keep_all){
    distinct_locs <- GRP_starts(group_info)
    N <- df_nrow(data)
    n_distinct_locs <- length(distinct_locs)
    if (.order){
      slice <- !(N == n_distinct_locs &&
                   isTRUE(attr(group_info[["order"]], "sorted")))
    } else {
      slice <- !(N == n_distinct_locs)
    }
    if (slice){
      out <- cheapr::sset_row(data, distinct_locs)
    } else {
      out <- data
    }
  } else {
    out <- GRP_groups(group_info)
  }
  if (identical(names(out), group_vars(data))){
    cheapr::reconstruct(out, f_ungroup(data))
  } else {
    cheapr::reconstruct(out, data)
  }
}
