#' Fast `dplyr::select()`/`dplyr::rename()`
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
  out <- df_select(data, pos)
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
  out <- cheapr::sset(df_ungroup(data), j = unname(pos))
  names(out) <- names(pos)
  attr(out, "groups") <- groups
  class(out) <- class(data)
  # class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
#' @export
f_select.data.table <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- df_select(data, pos)
  names(out) <- names(pos)
  keys <- attr(data, "sorted")
  out <- collapse::qDT(out)
  if (all(keys %in% names(out))){
    if (all(cpp_address_equal(df_select(out, keys), df_select(data, keys)))){
      attr(out, "sorted") <- keys
    }
  }
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
  col_rename(data, .cols = pos)
}
#' @export
f_rename.grouped_df <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  groups <- group_data(data)
  group_vars <- setdiff(names(groups), ".rows")
  # Rename data columns
  out <- col_rename(df_ungroup(data), .cols = pos)
  # Rename group data columns
  group_pos <- which(group_vars %in% names(data)[pos])
  names(group_pos) <- names(out)[which(names(out) %in% names(pos) &
                                          names(data) %in% group_vars)]
  groups <- col_rename(groups, .cols = group_pos)
  attr(out, "groups") <- groups
  class(out) <- class(data)
  # class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
# This should be unecessary but data.table:::`names<-.data.table`
# Sometimes reduces the allocated column slots
#' @export
f_rename.data.table <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- col_rename(data, .cols = pos)
  collapse::qDT(out)
}
