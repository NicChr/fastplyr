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
  # cli::cli_warn(
  #   c("i" = "{.fn add_group_id} has been superseded by the use of",
  #     "{.code f_mutate(data, group_id = cur_group_id()}"),
  #   .frequency = "once", .frequency_id = "deprecate_add_group_id"
  # )
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

  .name <- .name %||% unique_col_name(names(data), "group_id")

  ids <- GRP_group_id(GRP)
  if (as_qg){
    ids <- group_id_to_qg(ids,
                          n_groups = GRP_n_groups(GRP),
                          group_sizes = GRP_group_sizes(GRP),
                          group_starts = GRP_starts(GRP),
                          ordered = .order)
  }
  df_add_col(.data, .name, ids)
}
#' @rdname add_id
#' @export
add_row_id <- function(.data, ...){
#   cli::cli_warn(
#     c("i" = "{.fn add_row_id} has been superseded by the use of",
#       "{.code f_mutate(data, row_id = row_number()}"),
#     .frequency = "once", .frequency_id = "deprecate_add_row_id"
#   )
  UseMethod("add_row_id")
}
#' @rdname add_id
#' @export
add_row_id.data.frame <- function(.data, ...,
                                  .ascending = TRUE,
                                  .by = NULL, .cols = NULL,
                                  .name = NULL){
  if (is.null(.name)){
    .name <- unique_col_name(names(.data), "row_id")
  }
  N <- df_nrow(.data)
  group_info <- tidy_group_info(.data, ..., .by = {{ .by }},
                                .cols = .cols)
  # data <- group_info[["data"]]
  extra_groups <- group_info[["extra_groups"]]
  group_vars <- group_info[["dplyr_groups"]]
  groups_changed <- group_info[["groups_changed"]]
  all_groups <- group_info[["all_groups"]]
  if (length(all_groups) == 0L){
    if (.ascending){
      row_ids <- seq_len(N)
    } else {
      row_ids <- seq.int(length.out = N, from = N, by = -1L)
    }
  } else {
    if (length(extra_groups) == 0 &&
        length(group_vars) == length(group_vars(.data)) &&
        !groups_changed){
      groups <- group_data(.data)
      sizes <- cheapr::list_lengths(groups[[".rows"]])
      o <- cpp_unlist_group_locs(groups[[".rows"]], sizes)
      row_ids <- cpp_row_id(o, sizes, .ascending)
    } else {
      row_ids <- row_id(
        f_select(group_info[["data"]], .cols = all_groups),
        ascending = .ascending
      )
    }
  }
  col_to_add <- add_names(list(row_ids), .name)
  cheapr::df_modify(.data, col_to_add)
}
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
  group_info <- tidy_group_info(.data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  extra_groups <- group_info[["extra_groups"]]
  group_vars <- group_info[["dplyr_groups"]]
  all_groups <- group_info[["all_groups"]]
  temp <- group_info[["data"]]

  if (length(extra_groups) == 0){
    ids <- rep_len(1L, df_nrow(.data))
  } else if (length(group_vars) == 0){
    ids <- f_consecutive_id(f_select(temp, .cols = extra_groups))
  } else {
    group_ids <- add_group_id(temp, .order = .order,
                              .cols = all_groups)[[df_ncol(temp) + 1L]]
    o <- radixorderv2(f_select(temp, .cols = group_vars),
                      group.sizes = TRUE,
                      sort = .order)
    sizes <- attr(o, "group.sizes")
    ids <- cpp_grouped_run_id(group_ids, o, sizes)
  }
  if (is.null(.name)){
    .name <- unique_col_name(names(.data), "consecutive_id")
  }
  col_to_add <- add_names(list(ids), .name)
  cheapr::df_modify(.data, col_to_add)
}
