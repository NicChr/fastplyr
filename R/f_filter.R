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
  group_info <- mutate_summary(data, ..., .by = {{ .by }})
  filter_cols <- group_info[["new_cols"]]
  filter_df <- cheapr::sset_col(group_info[["data"]], filter_cols)
  if (df_ncol(filter_df) < 1){
    data
  } else {
    if (!all(vapply(filter_df, is.logical, FALSE))){
      cli::cli_abort("All expressions in {.fn f_filter} must be logical vectors")
    }
    cheapr::sset_df(data, cpp_which_all(filter_df))
  }
}

