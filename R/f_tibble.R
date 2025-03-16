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
  out <- list_tidy(..., .keep_null = FALSE, .named = TRUE)

  if (.recycle) {
    out <- do.call(function(...) cheapr::recycle(..., length = .nrows), out)
  }
  if (is.null(.nrows)) {
    if (length(out) == 0L) {
      row_names <- integer()
    }
    else {
      N <- NROW(.subset2(out, 1L))
      row_names <- c(NA_integer_, -N)
    }
  }
  else {
    row_names <- .set_row_names(.nrows)
  }
  out_names <- as.character(attr(out, "names", TRUE))
  if (.name_repair) {
    out_names <- unique_name_repair(out_names)
  }
  attr(out, "names") <- out_names
  attr(out, "row.names") <- row_names
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
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
    out <- list_rm_null(list(name = names(x), value = x))
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
    empty <- empty_str_locs(names(out))
    if (length(empty) > 0){
      names(out)[empty] <- paste0("col_", empty)
    }
  }
  out
}

fast_tbl <- function(...){
  df_as_tbl(cheapr::fast_df(...))
}
