#' Fast 'tibble' alternatives
#'
#' @param x A data frame or vector.
#' @param name `character(1)` Name to use for column of names.
#' @param value `character(1)` Name to use for column of values.
#' @param ... Dynamic name-value pairs.
#' @param .nrows `integer(1)` (Optional) number of rows. \cr
#' Commonly used to initialise a 0-column data frame with rows.
#' @param .recycle `logical(1)` Should arguments be recycled?
#' Default is `FALSE`.
#' @param .name_repair `logical(1)` Should duplicate names be made unique?
#' Default is `TRUE`.
#'
#' @returns
#' A tibble or vector.
#'
#' @details
#' `new_tbl` and `as_tbl` are alternatives to
#' `tibble` and `as_tibble` respectively.
#'
#' `f_enframe(x)` where `x` is a `data.frame` converts `x` into a tibble
#' of column names and list-values.
#'
#' @rdname new_tbl
#' @export
new_tbl <- function (..., .nrows = NULL, .recycle = TRUE, .name_repair = TRUE){
  df_as_tbl(cpp_new_df(list_tidy(..., .named = TRUE), .nrows, .recycle, .name_repair))
}
#' @rdname new_tbl
#' @export
f_enframe <- function(x, name = "name", value = "value"){
  if (inherits(x, "data.frame")) {
    x <- unclass(x)
    attr(x, "row.names") <- NULL
  }
  x_nms <- names(x)
  x <- unname(x)
  if (is.null(x_nms)) {
    out <- list(x)
    names(out) <- value
  }
  else {
    out <- list(x_nms, x)
    names(out) <- c(name, value)
  }
  as_tbl(out)
}
#' @rdname new_tbl
#' @export
f_deframe <- function(x){
  ncol <- length(names(x))
  if (!(inherits(x, "data.frame") && ncol %in% (1:2))) {
    stop("`x` must be a 1 or 2 col data frame")
  }
  out <- .subset2(x, ncol)
  if (ncol == 2) {
    names(out) <- as.character(.subset2(x, 1L))
  }
  out
}
#' @rdname new_tbl
#' @export
as_tbl <- function(x){
  df_as_tbl(cheapr::as_df(x))
}

fast_tbl <- function(...){
  df_as_tbl(cheapr::fast_df(...))
}
