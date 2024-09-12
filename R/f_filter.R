#' Alternative to `dplyr::filter()`
#'
#' @param data A data frame.
#' @param ... Expressions used to filter the data frame with.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#'
#' @returns
#' A filtered data frame.
#'
#' @export
f_filter <- function(data, ..., .by = NULL){
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                ungroup = FALSE,
                                rename = TRUE)
  filter_cols <- group_info[["extra_groups"]]
  filter_df <- cheapr::sset(df_ungroup(group_info[["data"]]), j = filter_cols)
  if (df_ncol(filter_df) < 1){
    data
  } else {
    stopifnot(all(vapply(filter_df, is.logical, FALSE)))
    df_row_slice(data, cpp_which_all(filter_df))
  }
}

