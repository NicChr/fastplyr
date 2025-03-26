#' Fast remove rows with `NA` values
#'
#' @param data A data frame.
#' @param ... Cols to fill `NA` values specified through `tidyselect` notation.
#' If left empty all cols are used by default.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A data frame with removed rows containing either any or all `NA` values.
#'
#' @rdname remove_na
#' @export
remove_rows_if_any_na <- function(data, ..., .cols = NULL){
  if (is.null(.cols) && dots_length(...) == 0){
    .cols <- names(data)
  }
  na_data <- f_select(data, ..., .cols = .cols)
  drop <- cheapr::row_any_na(na_data)
  keep <- cheapr::val_find(drop, FALSE)
  if (length(keep) == df_nrow(data)){
    data
  } else {
    cheapr::sset_df(data, keep)
  }
}
#' @rdname remove_na
#' @export
remove_rows_if_all_na <- function(data, ..., .cols = NULL){
  if (is.null(.cols) && dots_length(...) == 0){
    .cols <- names(data)
  }
  na_data <- f_select(data, ..., .cols = .cols)
  drop <- cheapr::row_all_na(na_data)
  keep <- cheapr::val_find(drop, FALSE)
  if (length(keep) == df_nrow(data)){
    data
  } else {
    cheapr::sset_df(data, keep)
  }
}
