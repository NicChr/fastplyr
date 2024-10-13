#' Consecutive IDs
#'
#' @description
#' `f_consecutive_id()`, an alternative to `dplyr::consecutive_id()`
#' creates an integer vector with values in the range `[1, n]` where
#' `n` is the length of the vector or number of rows of the data frame.
#' The ID increments every time `x[i] != x[i - 1]` thus giving information on
#' when there is a change in value.
#'
#'
#' @param x A vector or data frame.
#' @param data A data frame.
#' @param ... Variables used to calculate consecutive IDs across.
#' They are specified using tidy `data-masking`. \cr
#' @param .by A tidy selection of groups used for a grouped calculation.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource or you are programming with
#' pre-defined character and numeric vectors it is recommended to use this.
#' @param .name Name of the added ID column which should be a
#' character vector of length 1.
#' If `NULL` (the default), `add_consecutive_id()`
#' will add a column named "consecutive_id".
#' @param .order Should the groups be calculated using an ordered algorithm?
#' In this case the output remains the same but setting this to `FALSE` can
#' many times offer a speed boost.
#'
#' @details
#' 'ALTREP' compact sequences are supported as well.
#'
#' To mimic `dplyr::consecutive_id()` where multiple variables are selected,
#' we can use `dplyr::pick()` within `mutate()` or simply use
#' `add_consecutive_id()`.
#'
#' `f_consecutive_id` has a smaller overhead and thus should be faster when
#' called many times, e.g. when using a `grouped_df` with many groups.
#'
#' `add_consecutive_id` is somewhat faster when there are many groups,
#' e.g. >= 100,000.
#'
#' @returns
#' `f_consecutive_id` returns an
#' integer vector of consecutive run IDs in the range `[1, n]`.
#' `add_consecutive_id` adds a consecutive ID column.
#'
#' @examples
#' library(fastplyr)
#'
#' x <- rep(letters[1:3], each = 3)
#' run_id <- f_consecutive_id(x)
#' run_id
#'
#' # To get something similar to `rle()`
#' # We can just use `f_count()`
#'
#' new_tbl(id = run_id) %>%
#'   f_count(id, order = FALSE)
#'
#' # Using `add_consecutive_id()`
#' new_tbl(x) %>%
#'   add_consecutive_id(x) %>%
#'   f_count(consecutive_id, order = FALSE)
#' @rdname f_consecutive_id
#' @export
f_consecutive_id <- cpp_consecutive_id

#' @rdname f_consecutive_id
#' @export
add_consecutive_id <- function(data, ...){
  UseMethod("add_consecutive_id")
}
#' @rdname f_consecutive_id
#' @export
add_consecutive_id.data.frame <- function(data, ...,
                                          .by = NULL, .cols = NULL,
                                          .name = NULL,
                                          .order = df_group_by_order_default(data)){

  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  extra_groups <- group_info[["extra_groups"]]
  group_vars <- group_info[["dplyr_groups"]]
  all_groups <- group_info[["all_groups"]]
  temp <- group_info[["data"]]

  if (length(extra_groups) == 0){
    ids <- rep_len(1L, df_nrow(data))
  } else if (length(group_vars) == 0){
    ids <- f_consecutive_id(f_select(temp, .cols = extra_groups))
  } else {
    group_ids <- add_group_id(temp, .name = ".temp.group.id",
                              order = .order,
                              .cols = all_groups)[[".temp.group.id"]]
    o <- radixorderv2(f_select(temp, .cols = group_vars),
                      group.sizes = TRUE,
                      sort = .order)
    sizes <- attr(o, "group.sizes")
    ids <- cpp_grouped_run_id(group_ids, o, sizes)
  }
  if (is.null(.name)){
    .name <- unique_col_name(names(data), "consecutive_id")
  }
  col_to_add <- add_names(list(ids), .name)
  df_add_cols(data, col_to_add)
}
# add_consecutive_id.data.frame <- function(data, ...,
#                                           .by = NULL, .cols = NULL,
#                                           .name = NULL, .order = df_group_by_order_default(data)){
#
#   if (is.null(.name)){
#     .name <- unique_col_name(names(data), "consecutive_id")
#   }
#   N <- df_nrow(data)
#   group_info <- tidy_group_info(data, ..., .by = {{ .by }},
#                                 .cols = .cols,
#                                 ungroup = TRUE,
#                                 rename = FALSE,
#                                 dots_type = "tidyselect")
#   extra_groups <- group_info[["extra_groups"]]
#   group_vars <- group_info[["dplyr_groups"]]
#   all_groups <- group_info[["all_groups"]]
#
#   if (length(extra_groups) == 0){
#     ids <- rep_len(1L, N)
#   } else if (length(group_vars) == 0){
#     ids <- f_consecutive_id(f_select(data, .cols = extra_groups))
#   } else {
#     group_ids <- add_group_id(data, .name = ".temp.group.id",
#                               order = .order,
#                               .cols = all_groups)[[".temp.group.id"]]
#     o <- radixorderv2(f_select(data, .cols = group_vars), group.sizes = TRUE, sort = FALSE)
#     sizes <- attr(o, "group.sizes")
#
#     ids <- cpp_grouped_run_id(group_ids, o, sizes)
#   }
#   col_to_add <- add_names(list(ids), .name)
#   df_add_cols(data, col_to_add)
#
#   # if (is.null(.name)){
#   #   .name <- unique_col_name(names(data), "consecutive_id")
#   # }
#   # N <- df_nrow(data)
#   # group_vars <- group_vars(data)
#   # cols <- tidy_select_names(data, ..., .cols = .cols)
#   #
#   # if (length(cols) == 0){
#   #   ids <- rep_len(1L, N)
#   # } else if (length(group_vars) == 0){
#   #   ids <- f_consecutive_id(f_select(data, .cols = cols))
#   # } else {
#   #   group_ids <- add_group_id(data, .name = ".temp.group.id",
#   #                             .cols = c(group_vars, cols))[[".temp.group.id"]]
#   #   o <- radixorderv2(f_select(data, .cols = group_vars), group.sizes = TRUE)
#   #   sizes <- attr(o, "group.sizes")
#   #
#   #   ids <- cpp_grouped_run_id(group_ids, o, sizes)
#   # }
#   # col_to_add <- add_names(list(ids), .name)
#   # df_add_cols(data, col_to_add)
# }
