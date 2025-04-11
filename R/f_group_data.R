#' Fast group metadata
#'
#' @name group_data
#'
#' @param x A `data.frame` or `grouped_df`.
#'
#' @returns
#' Requested group metadata.
#'
#' @rdname group_data
#' @export
f_group_data <- cpp_group_data
#' @rdname group_data
#' @export
f_group_keys <- cpp_group_keys
#' @rdname group_data
#' @export
f_group_rows <- cpp_group_rows
#' @rdname group_data
#' @export
f_group_indices <- cpp_group_id
#' @rdname group_data
#' @export
f_group_size <- cpp_group_size
#' @rdname group_data
#' @export
f_n_groups <- function(x){
  df_nrow(cpp_group_keys(x))
}
