#' Find duplicate rows
#'
#' @param data A data frame.
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .both_ways If `TRUE` then duplicates and non-duplicate first instances
#' are retained. The default is `FALSE` which returns only duplicate rows. \cr
#' Setting this to `TRUE` can be particularly useful when examining
#' the differences between duplicate rows.
#' @param .add_count If `TRUE` then a count column is added to denote the
#' number of duplicates (including first non-duplicate instance).
#' The naming convention of this column follows `dplyr::add_count()`.
#' @param .drop_empty If `TRUE` then empty rows with all `NA` values are removed.
#' The default is `FALSE`.
#' @param sort Should result be sorted?
#' If `FALSE` (the default), then rows are returned in the exact same order as
#' they appear in the data.
#' If `TRUE` then the duplicate rows are sorted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of duplicate rows.
#'
#' @details
#' This function works like `dplyr::distinct()` in its handling of
#' arguments and data-masking but returns duplicate rows.
#' In certain situations in can be much faster than `data %>% group_by() %>% filter(n() > 1)`
#' when there are many groups.
#'
#' @seealso [f_count] [f_distinct]
#'
#' @rdname duplicate_rows
#' @export
f_duplicates <- function(data, ..., .keep_all = FALSE,
                         .both_ways = FALSE, .add_count = FALSE,
                         .drop_empty = FALSE, sort = FALSE,
                         .by = NULL, .cols = NULL){
  n_dots <- dots_length(...)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  all_groups <- group_info[["all_groups"]]
  out <- group_info[["data"]]
  out_nms <- names(out)
  # If no variables selected then all variables used
  if (n_dots == 0 && is.null(.cols)){
    dup_vars <- out_nms
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- out_nms
    } else {
      out_vars <- dup_vars
    }
  }
  if (length(group_info[["extra_groups"]]) == 0L && !group_info[["groups_changed"]]){
    out <- data
  }
  out <- f_select(out, .cols = out_vars)

  # Groups
  groups <- df_to_GRP(out, .cols = dup_vars,
                      return.order = FALSE,
                      return.groups = FALSE,
                      order = FALSE)
  if (.add_count){
    group_sizes <- GRP_expanded_group_sizes(groups)
    n_var_nm <- unique_count_col(out)
    out[[n_var_nm]] <- group_sizes
  }
  which_dup <- GRP_which_duplicated(groups, all = .both_ways)
  out <- cheapr::sset(out, which_dup)
  if (sort){
    out <- f_arrange(out, .cols = dup_vars)
  }

  # Remove empty rows (rows with all NA values)

  if (.drop_empty){
    out <- df_drop_empty(out, .cols = dup_vars)
  }

  # Adjust group sizes as they reflect the dup count + 1

  if (.add_count && !.both_ways && df_nrow(out) > 0){
    cheapr::set_subtract(out[[n_var_nm]], 1L)
    which_zero <- cheapr::which_val(out[[n_var_nm]], 0L)
    collapse::setv(
      out[[n_var_nm]],
      which_zero,
      1L,
      vind1 = TRUE
    )
  }

  reconstruct(data, out)
}
