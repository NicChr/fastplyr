#' A `collapse` version of `dplyr::arrange()`
#'
#' @description
#' This is a fast and near-identical alternative to `dplyr::arrange()`
#' using the `collapse` package.
#'
#' `desc()` is like `dplyr::desc()` but works faster when
#' called directly on vectors. \cr
#'
#' @param .data A data frame.
#' @param ... Variables to arrange by.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .by_group If `TRUE` the sorting will be first done by the group
#' variables.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .descending `[logical(1)]` data frame be arranged in descending order? Default is
#' `FALSE`. In simple cases this can be easily achieved through `desc()` but
#' for a mixture of ascending and descending variables, it's easier to use
#' the `.descending` arg to reverse the order.
#' @param .in_place Should data be sorted in-place?
#' This can be very efficient for large data frames and can be safely used
#' when overwriting a freshly allocated data frame.
#' If you're unsure whether the data frame is a freshly allocated object,
#' use `cheapr::semi_copy()` before sorting.
#'
#' Please note that no new vectors and no copies are created,
#' data is directly sorted in-memory.
#' This only works on data frames consisting of atomic vectors.
#'
#' @returns
#' A sorted `data.frame`.
#'
#' @export
f_arrange <- function(.data, ..., .by = NULL, .by_group = FALSE,
                     .cols = NULL, .descending = FALSE, .in_place = FALSE){
  group_info <- tidy_dots_info(
    if (.by_group){
      .data
    } else {
      f_ungroup(.data)
    }, ..., .by = {{ .by }},
    .cols = .cols
  )
  dot_vars <- group_info[["new_cols"]]
  group_vars <- group_info[["all_groups"]]
  if (length(dot_vars) == 0L){
    return(.data)
  }
  if (.by_group){
    order_vars <- c(group_vars, dot_vars)
  } else {
    order_vars <- dot_vars
  }

  if (.in_place){
    cli::cli_inform(
      c("{.arg .in_place} has been set to `TRUE` and will arrange that data
     in-place.",
        "Please be aware this may sort all objects that point to the same
     underlying data")
    )

    is_exotic <- vapply(.data, \(x) cpp_is_exotic(x) && !rlang::is_bare_list(x), TRUE)

    if (any(is_exotic)){

      exotic_vars <- names(is_exotic)[is_exotic]

      cli::cli_abort(
      c("Cannot arrange `.data` in-place as it contains the following exotic variables:",
        exotic_vars)
      )
    }
    if (length(group_info[["changed_cols"]])){
      cli::cli_abort(
        c("Cannot arrange `.data` in-place as the following variables have been modified:",
          group_info[["changed_cols"]])
      )
    }
    if (.descending){
      df_set_order(group_info[["data"]], order_vars, .order = -1L)
    } else {
      df_set_order(group_info[["data"]], order_vars, .order = 1L)
    }

    .data <- cheapr::rebuild(cheapr::as_df(.data), .data)

    # Rebuild attributes and make sure it's from a fresh source without attributes
    # as rebuilding grouped_df relies on comparing memory addresses
    # which haven't been touched when we sort in-place
    if (length(f_group_vars(.data))){
      .data <- f_group_by(.data, .cols = f_group_vars(.data))
    }
    .data

  } else {
    out_order <- radixorderv2(
      cheapr::sset_col(
        group_info[["data"]], order_vars
      ),
      decreasing = .descending, na.last = TRUE, starts = FALSE,
      group.sizes = FALSE, sort = TRUE
    )
    sorted <- attr(out_order, "sorted")
    if (isTRUE(sorted)){
      .data
    } else {
      cheapr::sset(.data, out_order)
    }
  }
}
