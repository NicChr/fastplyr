#' Find duplicate rows
#'
#' @param .data A data frame.
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
#' @param .sort `r lifecycle::badge("deprecated")`  Use `.order` instead.
#' @param .order Should the groups be calculated as ordered groups?
#' Setting to `TRUE` here implies that the groups are returned sorted.
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
#' In certain situations in can be much faster than `data |> group_by()|> filter(n() > 1)`
#' when there are many groups.
#'
#' @seealso [f_count] [f_distinct]
#'
#' @rdname f_duplicates
#' @export
f_duplicates <- function(.data, ..., .keep_all = FALSE,
                         .both_ways = FALSE, .add_count = FALSE,
                         .drop_empty = FALSE,
                         .order = FALSE,
                         .sort = deprecated(),
                         .by = NULL, .cols = NULL){
  if (lifecycle::is_present(.sort)) {
    lifecycle::deprecate_warn("0.9.0", "f_duplicates(.sort = )", "f_duplicates(.order = )")
    .order <- .sort
  }
  if (dots_length(...) == 0 && is.null(.cols)){
    .cols <- names(.data)
  }
  group_info <- tidy_eval_groups(
    .data, ..., .by = {{ .by }}, .cols = .cols,
    .order = .order
  )

  out <- group_info[[1L]]
  GRP <- group_info[[2L]]

  dup_vars <- GRP_group_vars(GRP)

  if (!.keep_all){
    out <- cheapr::sset_col(out, dup_vars)
  }
  if (.add_count){
    group_sizes <- GRP_expanded_group_sizes(GRP)
    count_col <- unique_count_col(out)
    out <- cheapr::df_modify(out, list_tidy(!!count_col := group_sizes))
  }
  which_dup <- GRP_which_duplicated(GRP, all = .both_ways)

  # Neat way to return sorted duplicate rows

  if (.order){
    which_dup <- which_dup[order(GRP_group_id(GRP)[which_dup])]
  }
  out <- cheapr::sset_row(out, which_dup)

  # Remove empty rows (rows with all NA values)

  if (.drop_empty){
    out <- remove_rows_if_all_na(cheapr::sset_col(out, dup_vars))
  }

  # Adjust group sizes as they reflect the dup count + 1

  if (.add_count && !.both_ways){
    cheapr::set_subtract(out[[count_col]], 1L)
    which_zero <- cheapr::which_val(out[[count_col]], 0L)
    cpp_loc_set_replace(out[[count_col]], which_zero, 1L)
  }
  cheapr::rebuild(out, .data)
}
