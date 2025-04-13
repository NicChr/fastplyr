#' A fast replacement to dplyr::count()
#'
#' @description
#' Near-identical alternative to `dplyr::count()`.
#'
#' @param data A data frame.
#' @param ... Variables to group by.
#' @param wt Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param .order Should the groups be calculated as ordered groups?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#' If `TRUE` (the default), the groups are returned in sorted order,
#' exactly the same way as `dplyr::count`.
#' @param name The name of the new column in the output.
#'  If there's already a column called `n`,
#'  it will use `nn`.
#'  If there's a column called `n` and `n`n,
#'  it'll use `nnn`, and so on, adding `n`s until it gets a new name.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @details
#' This is a fast and near-identical alternative to dplyr::count() using the `collapse` package.
#' Unlike `collapse::fcount()`, this works very similarly to `dplyr::count()`.
#' The only main difference is that anything supplied to `wt`
#' is recycled and added as a data variable.
#' Other than that everything works exactly as the dplyr equivalent.
#'
#' `f_count()` and `f_add_count()` can be up to >100x faster than the dplyr equivalents.
#'
#' @returns
#' A `data.frame` of frequency counts by group.
#'
#' @export
f_count <- function(data, ..., wt = NULL, sort = FALSE,
                    .order = df_group_by_order_default(data),
                    name = NULL, .by = NULL, .cols = NULL){
  weights <- NULL
  wt_expr <- rlang::enquo(wt)
  if (!rlang::quo_is_null(wt_expr)){
    weights <- mutate_summary(data, .fastplyr.wt = !!wt_expr)[["data"]][[".fastplyr.wt"]]
  }

  if (dots_length(...) == 0 &&
      rlang::quo_is_null(rlang::enquo(.by)) && is.null(.cols) &&
      .order == df_group_by_order_default(data)){

    counts <- grouped_df_counts(data, weights = weights, expand = FALSE)
    group_vars <- group_vars(data)
    out <- group_keys(data)

  } else {

    group_info <- tidy_GRP(
      data, ...,
      .by = {{ .by }},
      .cols = .cols,
      .order = .order
    )

    out <- GRP_groups(group_info)
    group_vars <- GRP_group_vars(group_info)
    if (is.null(weights)){
      counts <- GRP_group_sizes(group_info)
    } else {
      counts <- collapse::fsum(
        as.double(weights),
        g = group_info,
        na.rm = TRUE,
        use.g.names = FALSE,
        fill = FALSE
      )
    }
    out <- cheapr::sset_col(out, group_vars)
  }
  count_col <- name %||% unique_count_col(out)
  out <- df_add_col(out, count_col, cheapr::na_replace(counts, 0L))
  if (sort){
    out <- f_arrange(out, .cols = count_col, .descending = TRUE)
  }
  if ((length(group_vars(data)) + 1L) == df_ncol(out)){
    cheapr::reconstruct(out, f_ungroup(data))
  } else {
    cheapr::reconstruct(out, data)
  }
}
f_add_count <- function(data, ..., wt = NULL, sort = FALSE,
                        .order = df_group_by_order_default(data),
                        name = NULL, .by = NULL, .cols = NULL){
  weights <- NULL
  wt_expr <- rlang::enquo(wt)
  if (!rlang::quo_is_null(wt_expr)){
    weights <- mutate_summary(data, .fastplyr.wt = !!wt_expr)[["data"]][[".fastplyr.wt"]]
  }

  if (dots_length(...) == 0 &&
      rlang::quo_is_null(rlang::enquo(.by)) && is.null(.cols) &&
      .order == df_group_by_order_default(data)){

    counts <- grouped_df_counts(data, weights = weights, expand = TRUE)
    group_vars <- group_vars(data)
    out <- data
  } else {

    group_info <- tidy_GRP(
      data, ...,
      .by = {{ .by }},
      .cols = .cols,
      .order = .order
    )
    out <- GRP_data(group_info)
    group_vars <- GRP_group_vars(group_info)
    if (is.null(weights)){
      counts <- GRP_group_sizes(group_info)[GRP_group_id(group_info)]
    } else {
      counts <- collapse::fsum(
        as.double(weights),
        g = group_info,
        na.rm = TRUE,
        use.g.names = FALSE,
        fill = FALSE,
        TRA = "replace_fill"
      )
    }
  }
  count_col <- name %||% unique_count_col(data)
  out <- df_add_col(out, count_col, cheapr::na_replace(counts, 0L))
  if (sort){
    out <- f_arrange(out, .cols = count_col, .descending = TRUE)
  }
  cheapr::reconstruct(out, data)
}
