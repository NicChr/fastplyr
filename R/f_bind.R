#' Bind data frame rows and columns
#'
#' @name f_bind
#'
#' @description
#' Faster bind rows and columns.
#'
#' @param ... Data frames to bind.
#' @param .repair_names Should duplicate column names be made unique?
#' Default is `TRUE`.
#' @param .recycle Should inputs be recycled to a common row size?
#' Default is `TRUE`.
#'
#' @returns
#' `f_bind_rows()` performs a union of the data frames specified via `...` and
#' joins the rows of all the data frames, without removing duplicates.
#'
#' `f_bind_cols()` joins the columns, creating unique column names if there are
#' any duplicates by default.
#'
#'
#' @rdname f_bind
#' @export
f_bind_rows <- function(...){
  dots <- cpp_as_list_of_frames(list_rm_null(as_list_of(...)))
  if (length(dots) == 0){
    new_tbl()
  } else {
    out <- cpp_c(dots)
    names(out) <- cheapr::name_repair(names(out))
    out
  }
}
#' @rdname f_bind
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .recycle = TRUE){
  frames <- cpp_as_list_of_frames(list_rm_null(as_list_of(...)))
  if (length(frames)){
    cpp_df_col_c(frames, .recycle, .repair_names)
  } else {
    new_tbl()
  }
}
