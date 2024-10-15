#' Helpers to sort variables in ascending or descending order
#'
#' @description An alternative to `dplyr::desc()` which is much faster
#' for character vectors and factors.
#'
#' @param x Vector.
#'
#' @returns
#' A numeric vector that can be ordered in ascending or descending order. \cr
#' Useful in `dplyr::arrange()` or `f_arrange()`.
#'
#' @rdname desc
#' @export
desc <- function(x){
  -asc(x)
}
asc <- function(x){
  if (is.numeric(x) && !is.object(x)){
    x
  } else {
    strip_attrs(quick_group(x, order = TRUE, na_exclude = TRUE))
  }
}
