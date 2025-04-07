#' 'collapse' version of `dplyr::group_by()`
#'
#' @description
#' This works the exact same as `dplyr::group_by()` and typically
#' performs around the same speed but uses slightly less memory.
#'
#' @param data data frame.
#' @param ... Variables to group by.
#' @param .add Should groups be added to existing groups?
#' Default is `FALSE`.
#' @param .order Should groups be ordered? If `FALSE`
#' groups will be ordered based on first-appearance. \cr
#' Typically, setting order to `FALSE` is faster.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .drop Should unused factor levels be dropped? Default is `TRUE`.
#'
#'
#' @returns
#' `f_group_by()` returns a `grouped_df` that can be used
#' for further for grouped calculations.
#'
#' `group_ordered()` returns `TRUE` if the group data are sorted,
#' i.e if `attr(attr(data, "groups"), "ordered") == TRUE`. If sorted,
#' which is usually the default, this leads to summary calculations
#' like `f_summarise()` or `dplyr::summarise()` producing sorted groups.
#' If `FALSE` they are returned based on order-of-first appearance in the data.
#'
#' @details
#' `f_group_by()` works almost exactly like the 'dplyr' equivalent.
#' An attribute "ordered" (`TRUE` or `FALSE`) is added to the group data to
#' signify if the groups are sorted or not.
#'
#' ### Ordered vs Sorted
#'
#' The distinction between ordered and sorted is somewhat subtle.
#' Functions in fastplyr that use a `sort` argument generally refer
#' to the top-level dataset being sorted in some way, either by sorting
#' the group columns like in `f_expand()` or `f_distinct()`, or
#' some other columns, like the count column in `f_count()`.
#'
#' The `.order` argument, when set to `TRUE` (the default),
#' is used to mean that the group data will be calculated
#' using a sort-based algorithm, leading to sorted group data.
#' When `.order` is `FALSE`, the group data will be returned based on
#' the order-of-first appearance of the groups in the data.
#' This order-of-first appearance may still naturally be sorted
#' depending on the data.
#' For example, `group_id(1:3, order = T)` results in the same group IDs
#' as `group_id(1:3, order = F)` because 1, 2, and 3 appear in the data in
#' ascending sequence whereas `group_id(3:1, order = T)` does not equal
#' `group_id(3:1, order = F)`
#'
#'
#' Part of the reason for the distinction is that internally fastplyr
#' can in theory calculate group data
#' using the sort-based algorithm and still return unsorted groups,
#' though this combination is only available to the user in limited places like
#' `f_distinct(.order = TRUE, .sort = FALSE)`.
#'
#' The other reason is to prevent confusion in the meaning
#' of `sort` and `order` so that `order` always refers to the
#' algorithm specified, resulting in sorted groups, and `sort` implies a
#' physical sorting of the returned data. It's also worth mentioning that
#' in most functions, `sort` will implicitly utilise the sort-based algorithm
#' specified via `order = TRUE`.
#'
#'
#' ### Using the order-of-first appearance algorithm for speed
#'
#' In many situations (not all) it can be faster to use the
#' order-of-first appearance algorithm, specified via `.order = FALSE`.
#'
#' This can generally be accessed by first calling
#' `f_group_by(data, ..., .order = FALSE)` and then
#' performing your calculations.
#'
#' To utilise this algorithm more globally and package-wide,
#' set the '.fastplyr.order.groups' option to `FALSE` using the code:
#' `options(.fastplyr.order.groups = FALSE)`.
#'
#'
#'
#' @rdname f_group_by
#' @export
#'
f_group_by <- function(data, ..., .add = FALSE,
                       .order = df_group_by_order_default(data),
                       .by = NULL, .cols = NULL,
                       .drop = df_group_by_drop_default(data)){
  init_group_vars <- group_vars(data)
  check_by(data, {{ .by }})
  group_info <- tidy_group_info(
    cpp_ungroup(data), ...,
    .by = {{ .by }},
    .cols = .cols,
    ungroup = FALSE,
    rename = TRUE
  )
  out <- group_info[["data"]]
  groups <- group_info[["all_groups"]]
  if (.add){
    order_unchanged <- .order == df_group_by_order_default(data)
    drop_unchanged <- .drop == df_group_by_drop_default(data)
    no_extra_groups <- length(groups) == 0 || (length(fast_setdiff(groups, init_group_vars)) == 0)
    groups_unchanged <- all(group_info$address_equal[init_group_vars])
    if (order_unchanged && drop_unchanged && no_extra_groups && groups_unchanged){
      return(data)
    }
    groups <- unique(c(init_group_vars, groups))
  }
  if (length(groups) > 0L){
    groups <- group_collapse(out, .cols = groups,
                             order = .order,
                             id = FALSE,
                             loc = TRUE, sort = TRUE,
                             size = FALSE,
                             start = FALSE, end = FALSE,
                             .drop = .drop)
    groups <- f_rename(groups, .cols = c(".rows" = ".loc"))
    groups[[".rows"]] <- vctrs_new_list_of(groups[[".rows"]], integer())
    attr(groups, ".drop") <- .drop
    attr(groups, "ordered") <- .order
    attr(out, "groups") <- groups
    class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  out
}
#' @rdname f_group_by
#' @export
group_ordered <- function(data){
  attr(group_data(data), "ordered") %||% TRUE
}
#' @rdname f_group_by
#' @export
f_ungroup <- df_ungroup
