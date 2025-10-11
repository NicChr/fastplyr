#' Add a column of useful IDs (group IDs, row IDs & consecutive IDs)
#'
#' @param .data A data frame.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param .order Should the groups be ordered? \cr
#' When `.order` is `TRUE` (the default) the group IDs will be
#' ordered but not sorted. \cr
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param .ascending Should the order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `add_row_id()` this determines if the
#' row IDs are in increasing or decreasing order. \cr
#' \bold{NOTE} - When `order = FALSE`, the `ascending` argument is
#' ignored. This is something that will be fixed in a later version.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .name Name of the added ID column which should be a
#' character vector of length 1.
#' If `.name = NULL` (the default),
#' `add_group_id()` will add a column named "group_id",
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#'
#' @returns
#' A data frame with the requested ID column.
#'
#' @seealso [group_id] [row_id] [f_consecutive_id]
#'
#' @rdname add_id
#' @export
add_group_id <- function(.data, ...){
  UseMethod("add_group_id")
}
#' @rdname add_id
#' @export
add_group_id.data.frame <- function(.data, ...,
                                    .order = group_by_order_default(.data),
                                    .ascending = TRUE,
                                    .by = NULL, .cols = NULL,
                                    .name = NULL,
                                    as_qg = FALSE){
  N <- df_nrow(.data)
  check_by(.data, .by = {{ .by }})
  group_info <- tidy_eval_groups(.data, ..., .by = {{ .by }},
                                 .cols = .cols,
                                 .order = .order)
  data <- group_info[[1L]]
  GRP <- group_info[[2L]]
  all_groups <- GRP_group_vars(GRP)

  ids <- GRP_group_id(GRP)
  if (as_qg){
    ids <- group_id_to_qg(ids,
                          n_groups = GRP_n_groups(GRP),
                          group_sizes = GRP_group_sizes(GRP),
                          group_starts = GRP_starts(GRP),
                          ordered = .order)
  }

  .name <- .name %||% unique_col_name(names(.data), "group_id")
  df_add_col(.data, .name, ids)
}
#' @rdname add_id
#' @export
add_row_id <- function(.data, ...){
  UseMethod("add_row_id")
}
#' @rdname add_id
#' @export
add_row_id.data.frame <- function(.data, ...,
                                  .ascending = TRUE,
                                  .by = NULL, .cols = NULL,
                                  .name = NULL){

  N <- df_nrow(.data)

  group_info <- tidy_dots_info(
    .data, ..., .by = {{ .by }},
    .cols = .cols, .order = FALSE
  )

  group_vars <- get_groups(.data, .by = {{ .by }})
  dot_vars <- group_info[["used_cols"]]
  all_vars <- c(vec_setdiff(group_vars, dot_vars), dot_vars)

  data <- group_info[["data"]]

  # Plain row numbers
  if (length(all_vars) == 0L){
    if (.ascending){
      row_ids <- seq_len(N)
    } else {
      row_ids <- seq.int(length.out = N, from = N, by = -1L)
    }
  } else {
    row_ids <- row_id(
      cheapr::sset_col(data, all_vars),
      ascending = .ascending
    )
  }
  .name <- .name %||% unique_col_name(names(.data), "row_id")
  cheapr::df_modify(.data, list_tidy(!!.name := row_ids))
}

# Alternative (a bit slower)
# add_row_id.data.frame <- function(.data, ...,
#                                   .ascending = TRUE,
#                                   .by = NULL, .cols = NULL,
#                                   .name = NULL){
#   if (is.null(.name)){
#     .name <- unique_col_name(names(.data), "row_id")
#   }
#
#   N <- df_nrow(.data)
#
#   group_info <- tidy_eval_groups(
#     .data, ..., .by = {{ .by }},
#     .cols = .cols, .order = FALSE
#   )
#
#   GRP <- group_info[["GRP"]]
#   group_ids <- GRP_group_id(GRP)
#   n_groups <- GRP_n_groups(GRP)
#
#   # Plain row numbers
#   if (n_groups <= 1){
#     if (.ascending){
#       row_ids <- seq_len(N)
#     } else {
#       row_ids <- seq.int(length.out = N, from = N, by = -1L)
#     }
#   } else {
#     row_ids <- row_id(group_ids, ascending = .ascending)
#   }
#   cheapr::df_modify(.data, list_tidy(!!.name := row_ids))
# }

#' @rdname add_id
#' @export
add_consecutive_id <- function(.data, ...){
  UseMethod("add_consecutive_id")
}
#' @rdname add_id
#' @export
add_consecutive_id.data.frame <- function(.data, ...,
                                          .order = group_by_order_default(.data),
                                          .by = NULL, .cols = NULL,
                                          .name = NULL){

  N <- df_nrow(.data)

  group_info <- tidy_dots_info(
    .data, ..., .by = {{ .by }},
    .cols = .cols, .order = FALSE
  )

  group_vars <- get_groups(.data, .by = {{ .by }})
  dot_vars <- group_info[["new_cols"]]
  all_vars <- c(vec_setdiff(group_vars, dot_vars), dot_vars)

  data <- group_info[["data"]]
  GRP <- group_info[["GRP"]]

  if (length(dot_vars) == 0){
    ids <- cheapr::cheapr_rep_len(1L, N)
  } else if (length(group_vars) == 0){
    ids <- f_consecutive_id(cheapr::sset_col(data, all_vars))
  } else {
    o <- GRP_order(GRP)
    sizes <- GRP_group_sizes(GRP)
    group_ids <- group_id(cheapr::sset_col(data, dot_vars))
    ids <- cpp_grouped_run_id(group_ids, o, sizes)
  }

  .name <- .name %||% unique_col_name(names(.data), "consecutive_id")
  cheapr::df_modify(.data, list_tidy(!!.name := ids))
}


