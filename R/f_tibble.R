#' Fast 'tibble' alternatives
#'
#' @param x A data frame or vector.
#' @param name `character(1)` Name to use for column of names.
#' @param value `character(1)` Name to use for column of values.
#' @param ... Key-value pairs.
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
#' One of the main reasons that these do not share the same name prefixed with
#' `f_` is because they don't always return the same result. For example
#' `new_tbl()` does not support 'quosures' and tidy injection.
#'
#' `f_enframe(x)` where `x` is a `data.frame` converts `x` into a tibble
#' of column names and list-values.
#'
#' @rdname new_tbl
#' @export
new_tbl <- function(..., .nrows = NULL, .recycle = TRUE, .name_repair = FALSE){
  df_as_tbl(new_df(..., .nrows = .nrows, .recycle = .recycle, .name_repair = .name_repair))
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
  class(out) <- c("tbl_df", "tbl", "data.frame")
  attr(out, "row.names") <- .set_row_names(length(x))
  out
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
  if (is_df(x)){
    out <- df_as_tbl(x)
  } else if (is.null(x) || (is.atomic(x) && length(dim(x)) < 2)){
    out <- list3(name = names(x), value = x)
    attr(out, "row.names") <- .set_row_names(NROW(x))
    class(out) <- c("tbl_df", "tbl", "data.frame")
  } else {
    # Plain list
    if (!is.object(x) && is.list(x)){
      out <- list_as_tbl(do.call(cheapr::recycle, as.list(x)))
    } else {
      out <- df_as_tbl(
        as.data.frame(x, stringsAsFactors = FALSE)
      )
    }
    if (is.null(names(out))){
      names(out) <- paste0("col_", seq_along(out))
    }
    non_empty <- nzchar(names(out))
    if (!all(non_empty)){
      empty <- cheapr::which_(non_empty, invert = TRUE)
      names(out)[empty] <- paste0("col_", empty)
    }
  }
  out
}
