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
  frames <- cpp_as_list_of_frames(tidy_as_list_of(...))
  if (length(frames)){
    out <- cheapr::cheapr_c(.args = frames)
    names(out) <- cheapr::name_repair(names(out))
    out
  } else {
    new_tbl()
  }
}
#' @rdname f_bind
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .recycle = TRUE){
  frames <- cpp_as_list_of_frames(tidy_as_list_of(...))
  if (length(frames)){
    cheapr::col_c(.args = frames, .recycle = .recycle, .name_repair = .repair_names)
  } else {
    new_tbl()
  }
}
