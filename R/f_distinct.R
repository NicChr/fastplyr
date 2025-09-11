#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#'
#' @param .data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
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
#' A `data.frame` of distinct groups.
#'
#' @export
f_distinct <- function(.data, ..., .keep_all = FALSE,
                       .order = FALSE,
                       .sort = deprecated(),
                       .by = NULL, .cols = NULL){

  if (lifecycle::is_present(.sort)) {
    lifecycle::deprecate_warn("0.9.0", "f_distinct(.sort = )", "f_distinct(.order = )")
    .order <- .sort
  }

  if (dots_length(...) == 0 && is.null(.cols)){
    .cols <- names(.data)
  }
  group_info <- tidy_eval_groups(
    .data, ..., .by = {{ .by }},
    .cols = .cols, .order = .order
  )
  data <- group_info[[1L]]
  GRP <- group_info[[2L]]

  if (.keep_all){
    distinct_locs <- GRP_starts(GRP)
    N <- df_nrow(data)
    n_distinct_locs <- length(distinct_locs)
    if (.order){
      slice <- !(N == n_distinct_locs &&
                   isTRUE(attr(GRP[["order"]], "sorted")))
    } else {
      slice <- !(N == n_distinct_locs)
    }
    if (slice){

      # 1. Choose all cols in the data
      # not already covered by the user's chosen cols
      # 2. Filter on unique locations
      # 3. bind this onto the unique data containing the rest of the columns
      # 4. Re-order the columns as needed
      out <- cheapr::sset_col(
        f_bind_cols(
          cheapr::sset_row(
            cheapr::sset_col(data, vec_setdiff(names(data), GRP_group_vars(GRP))),
            distinct_locs
          ),
          GRP_groups(GRP)
        ),
        names(data)
      )
    } else {
      out <- data
    }
  } else {
    out <- GRP_groups(GRP)
  }
  if (identical(names(out), group_vars(data))){
    cheapr::rebuild(out, f_ungroup(data))
  } else {
    cheapr::rebuild(out, data)
  }
}
