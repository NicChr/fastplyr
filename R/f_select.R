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
  group_vars <- group_vars(data)
  group_data <- group_data(data)

  cols <- tidy_select_names(data, ..., .cols = .cols)

  missed_groups <- vec_setdiff(group_vars, cols)

  if (length(missed_groups) > 0){
    missed_groups_msg <- paste(missed_groups, collapse = ", ")
    cli::cli_inform(c("i" = "Adding missed group variables:", "{missed_groups_msg}"))
    cols <- c(`names<-`(missed_groups, missed_groups), cols)
  }

  out <- cheapr::sset_col(data, cols)
  names(out) <- names(cols)
  # out <- col_rename(out, cols)

  # If any groups have been renamed then rename the group data
  selected_group_vars <- vec_intersect(cols, group_vars)
  if (any(names(selected_group_vars) != selected_group_vars)){
    group_data <- col_rename(group_data, selected_group_vars)
    attr(out, "groups") <- group_data
  }
  out <- cheapr::rebuild(out, cpp_ungroup(data))
  attr(out, "groups") <- group_data
  class(out) <- c("grouped_df", class(out))
  out
}
#' @export
f_select.fastplyr_grouped_df <- function(data, ..., .cols = NULL){
  out <- NextMethod("f_select")
  GRP <- attr(data, "GRP")

  # Have groups been renamed?

  group_vars <- f_group_vars(out)
  grp_group_vars <- GRP_group_vars(GRP)
  renamed <- !is.null(grp_group_vars) && !identical(grp_group_vars, group_vars)

  if (renamed){
   GRP[["group.vars"]] <- group_vars
   grp_groups <- GRP_groups(GRP)
   names(grp_groups) <- group_vars
   GRP[["groups"]] <- grp_groups
  }

  attr(out, "GRP") <- GRP
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
  cheapr::rebuild(out, data)
}
#' @export
f_rename.grouped_df <- function(data, ..., .cols = NULL){
  group_vars <- group_vars(data)
  group_data <- group_data(data)

  cols <- tidy_select_names(data, ..., .cols = .cols)
  renamed_group_vars <- vec_intersect(cols, group_vars)

  out <- col_rename(cpp_ungroup(data), cols)

  if (length(renamed_group_vars) > 0L){
    group_data <- col_rename(group_data, renamed_group_vars)
    attr(out, "groups") <- group_data
  }
  out <- cheapr::rebuild(out, cpp_ungroup(data))
  attr(out, "groups") <- group_data
  class(out) <- c("grouped_df", class(out))
  out
}
#' @export
f_rename.fastplyr_grouped_df <- function(data, ..., .cols = NULL){
  out <- NextMethod("f_rename")
  GRP <- attr(data, "GRP")

  # Have groups been renamed?

  group_vars <- f_group_vars(out)
  grp_group_vars <- GRP_group_vars(GRP)
  renamed <- !is.null(grp_group_vars) && !identical(grp_group_vars, group_vars)

  if (renamed){
    GRP[["group.vars"]] <- group_vars
    grp_groups <- GRP_groups(GRP)
    names(grp_groups) <- group_vars
    GRP[["groups"]] <- grp_groups
  }

  attr(out, "GRP") <- GRP
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
