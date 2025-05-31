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
#' @param .new_names A name specification for the names of filled variables.
#' The default `"{.col}"` replaces the given variables with the imputed ones.
#' New variables can be created alongside the originals if we give a different
#' specification, e.g.
#' `.new_names = "{.col}_imputed"`.
#' This follows the specification of `dplyr::across` if `.fns` were an empty
#' string `""`.
#'
#' @returns
#' A data frame with `NA` values filled forward or backward.
#'
#' @export
f_fill <- function(data, ..., .by = NULL, .cols = NULL,
                   .direction = c("forwards", "backwards"),
                   .fill_limit = Inf,
                   .new_names = "{.col}"){
  .direction <- rlang::arg_match(.direction)
  locf <- .direction == "forwards"
  nrows <- df_nrow(data)
  if (is.null(.cols) && rlang::dots_n(...) == 0){
    fill_cols <- names(data)
  } else {
    fill_cols <- unname(tidy_select_names(data, ..., .cols = .cols))
  }
  group_vars <- get_groups(data, .by = {{ .by }})
  groups <- f_select(data, .cols = group_vars)
  data_to_fill <- f_select(f_ungroup(data),
                           .cols = vec_setdiff(fill_cols, group_vars))

  if (df_ncol(groups) == 0){
    if (locf){
      o <- seq_len(nrows)
    } else {
      o <- seq(from = nrows, length.out = nrows, by = -1L)
    }
    sizes <- nrows
  } else {
    # Use group metadata and just unlist `group_rows(data)`
    # `cpp_unlist_group_locs()` is a dedicated function to do this
    if (identical(group_vars, group_vars(data))){
      group_data <- group_data(data)
      sizes <- cheapr::list_lengths(group_data[[".rows"]])
      o <- cpp_unlist_group_locs(group_data[[".rows"]], sizes)
    } else {
      o <- radixorderv2(groups, group.sizes = TRUE,
                        sort = FALSE, decreasing = FALSE)
      sizes <- attr(o, "group.sizes")
    }
    # Note: If we simply did `order(decreasing = TRUE)`
    # This ordering vector is not correct as it
    # places ties in order-of-first appearance when
    # we need ties in reverse order
    if (!locf){
      # Reverse vector without copy (by reference)
      rev_in_place <- function(x){
        cheapr_cpp_rev(x, set = TRUE)
      }
      o <- rev_in_place(o)
      sizes <- rev_in_place(sizes)
    }
  }

  data_to_fill <- cpp_fill_grouped(data_to_fill, o, sizes, .fill_limit)

  new_fill_names <- across_col_names(fill_cols, .fns = "", .names = .new_names)
  names(data_to_fill) <- new_fill_names
  cheapr::df_modify(data, data_to_fill)
}
