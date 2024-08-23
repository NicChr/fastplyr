#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#'
#' @param data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param sort Should result be sorted? Default is `FALSE`.
#' When `order = FALSE` this option has no effect on the result.
#' @param order Should the groups be calculated as ordered groups?
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
                      sort = FALSE, order = sort,
                      .by = NULL, .cols = NULL){
  n_dots <- dots_length(...)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  all_groups <- group_info[["all_groups"]]
  out <- group_info[["data"]]
  if (n_dots == 0 && is.null(.cols)){
    dup_vars <- names(out)
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- names(out)
    } else {
      out_vars <- dup_vars
    }
  }
  if (length(group_info[["extra_groups"]]) == 0L && !group_info[["groups_changed"]]){
    out <- data
  }
  out <- f_select(out, .cols = out_vars)
  # Using sort algorithm but returning order-of-first appearance groups
  if (order && !sort){
    unique_locs <- cheapr::which_val(row_id(f_select(out, .cols = dup_vars)), 1L)
    slice <- !(length(unique_locs) == df_nrow(out) && is_sorted(unique_locs))
  } else {
    if (order && sort){
      o <- radixorderv2(f_select(out, .cols = dup_vars), starts = TRUE)
      unique_locs <- o[attr(o, "starts")]
      slice <- !(length(unique_locs) == df_nrow(out) &&
                   isTRUE(attr(o, "sorted")))
    } else {
      groups <- group2(f_select(out, .cols = dup_vars))
      unique_locs <- attr(groups, "starts")
      slice <- !(length(unique_locs) == df_nrow(out))
    }
  }
  if (slice){
    out <- cheapr::sset(out, unique_locs)
  }
  reconstruct(data, out)
}
