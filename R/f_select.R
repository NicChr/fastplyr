#' Fast 'dplyr' `select()`/`rename()`/`pull()`
#'
#' @description
#' `f_select()` operates the exact same way as `dplyr::select()` and
#' can be used naturally with `tidy-select` helpers.
#' It uses collapse to perform the actual selecting of variables and is
#' considerably faster than dplyr for selecting exact columns,
#' and even more so when supplying the `.cols` argument.
#'
#' @param data A data frame.
#' @param ... Variables to select using `tidy-select`.
#' See `?dplyr::select` for more info.
#' @param .cols (Optional) faster alternative to `...` that accepts
#' a named character vector or numeric vector. \cr
#' No checks on duplicates column names are done when using `.cols`. \cr
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of selected columns.
#'
#' @export
f_select <- function(data, ..., .cols = NULL){
  UseMethod("f_select")
}
#' @export
f_select.data.frame <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- cheapr::sset_df(data, j = pos)
  names(out) <- names(pos)
  out
}
#' @export
f_select.grouped_df <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  group_vars <- group_vars(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  group_pos <- add_names(match(group_vars, data_nms), group_vars)
  pos_nms <- names(pos)
  # Add group vars missed
  groups_missed <- group_pos[match(group_pos, pos, 0L) == 0L]
  if (length(groups_missed) > 0L){
    text1 <- "Adding missing grouping variables: "
    message(
      paste0(text1,
             "'", paste(data_nms[groups_missed],
                        collapse = "', '"), "'")
    )
    pos <- c(groups_missed, pos)
    names(pos) <- c(data_nms[groups_missed], pos_nms)
  }
  renamed_groups <- pos[pos %in% group_pos &
                          !names(pos) %in% names(group_pos)]
  if (length(renamed_groups) > 0L){
    original_nms <- data_nms[unname(renamed_groups)]

    names(attr(data, "groups"))[
      match(original_nms,
            names(attr(data, "groups")))] <- names(renamed_groups)
  }
  groups <- group_data(data)
  out <- cheapr::sset(cpp_ungroup(data), j = unname(pos))
  names(out) <- names(pos)
  attr(out, "groups") <- groups
  class(out) <- class(data)
  # class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
#' @rdname f_select
#' @export
f_rename <- function(data, ..., .cols = NULL){
  UseMethod("f_rename")
}
#' @export
f_rename.data.frame <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- col_rename(data, .cols = pos)
  cheapr::reconstruct(out, data)
}
#' @export
f_rename.grouped_df <- function(data, ..., .cols = NULL){
  group_vars <- group_vars(data)
  group_data <- group_data(data)

  cols <- tidy_select_names(data, ..., .cols = .cols)
  renamed_group_vars <- fast_intersect(cols, group_vars)

  out <- col_rename(cpp_ungroup(data), cols)

  if (length(renamed_group_vars) > 0L){
    group_data <- col_rename(group_data, renamed_group_vars)
    attr(out, "groups") <- group_data
  }
  out <- cheapr::reconstruct(out, cpp_ungroup(data))
  attr(out, "groups") <- group_data
  class(out) <- c("grouped_df", class(out))
  out

}
#' @rdname f_select
#' @export
f_pull <- function(data, ..., .cols = NULL){
  col <- tidy_select_pos(data, ..., .cols = .cols)
  if (length(col) != 1){
    cli::cli_abort("You must select exactly one column")
  }
  .subset2(data, col)
}
#' @rdname f_select
#' @export
nothing <- function() character()
