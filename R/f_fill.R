#' Fill `NA` values forwards and backwards
#'
#' @param data A data frame.
#' @param ... Cols to fill `NA` values specified through `tidyselect` notation.
#' If left empty all cols are used by default.
#' @param .by Cols to group by for this operation.
#' Specified through `tidyselect`.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .direction Which direction should `NA` values be filled?
#' By default, "forwards" (Last-Observation-Carried-Forward) is used.
#' "backwards" is (Next-Observation-Carried-Backward).
#' @param .fill_limit The maximum number of consecutive `NA` values to fill.
#' Default is `Inf`.
#'
#' @returns
#' A data frame with `NA` values filled forward or backward.
#'
#' @export
f_fill <- function(data, ..., .by = NULL, .cols = NULL,
                   .direction = c("forwards", "backwards"),
                   .fill_limit = Inf){
  .direction <- rlang::arg_match(.direction)
  nrows <- df_nrow(data)
  if (is.null(.cols) && rlang::dots_n(...) == 0){
    fill_cols <- names(data)
  } else {
    fill_cols <- names(data)[
      match(tidy_select_pos(data, ..., .cols = .cols), seq_along(data))
    ]
  }
  group_vars <- get_groups(data, .by = {{ .by }})
  groups <- f_select(data, .cols = group_vars)
  data_to_fill <- f_select(f_ungroup(data),
                           .cols = fast_setdiff(fill_cols, group_vars))

  if (df_ncol(groups) == 0){
    if (.direction == "forwards"){
      o <- seq_len(nrows)
    } else {
      o <- seq(from = nrows, length.out = nrows, by = -1L)
    }
    sizes <- nrows
  } else {
    if (identical(group_vars, group_vars(data))){
      group_data <- group_data(data)
      o <- cpp_unlist_group_locs(group_data[[".rows"]])
      sizes <- cheapr::lengths_(group_data[[".rows"]])
      if (.direction == "backwards"){
        cheapr_cpp_rev <- get_from_package("cpp_rev", "cheapr")
        o <- cheapr_cpp_rev(o, set = TRUE)
        sizes <- cheapr_cpp_rev(sizes, set = TRUE)
      }
    } else {
      o <- radixorderv2(groups, group.sizes = TRUE, sort = FALSE,
                        decreasing = .direction == "backwards")
      sizes <- attr(o, "group.sizes")
    }
  }

  data_to_fill <- cpp_fill_grouped(data_to_fill, o, sizes, .fill_limit)
  df_modify_cols(data, data_to_fill)
}
