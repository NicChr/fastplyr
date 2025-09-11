#' Un-group `grouped_df`
#'
#' @param data A data frame.
#'
#' @returns
#' An un-grouped data frame.
#'
#' @rdname f_ungroup
#' @export
f_ungroup <- cpp_ungroup

#' @rdname f_ungroup
#' @export
group_ordered <- function(data){
  attr(f_group_data(data), "ordered") %||% TRUE
}
