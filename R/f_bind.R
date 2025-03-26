#' Bind data frame rows and columns
#'
#' @description
#' Faster bind rows and columns.
#'
#' @param ... Data frames to bind.
#' @param .repair_names Should duplicate column names be made unique?
#' Default is `TRUE`.
#' @param .sep Separator to use for creating unique column names.
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
#' @rdname f_bind_rows
#' @export
f_bind_rows <- function(...){
  dots <- cpp_as_list_of_frames(as_list_of(...))
  if (length(dots) == 0){
    new_tbl()
  } else {
    cpp_c(dots)
  }
}
#' @rdname f_bind_rows
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .recycle = TRUE, .sep = "..."){
  dots <- cpp_as_list_of_frames(list_rm_null(as_list_of(...)))

  if (length(dots) == 0){
    template <- list_as_tbl(list())
  } else {
    template <- dots[[1L]]
  }

  if (.recycle){
    dots <- cpp_recycle(dots, NULL)
  }

  nrows <- cpp_frame_dims(dots, check_rows_equal = !.recycle, check_cols_equal = FALSE)[[1L]]
  out <- as.list(unlist(unname(dots), recursive = FALSE))
  names(out) <- names(out) %||% character(length(out))

  if (.repair_names){
    names(out) <- unique_name_repair(names(out), .sep, .sep)
  }
  out <- list_as_df(out)
  if (length(dots) >= 1){
    N <- nrows[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
  }
  cheapr::reconstruct(out, template)
}
