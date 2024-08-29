#' Fast versions of `tidyr::expand()` and `tidyr::complete()`.
#'
#' @param data A data frame
#' @param ... Variables to expand
#' @param fill A named list containing value-name pairs
#' to fill the named implicit missing values.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of expanded groups.
#'
#' @details
#' `crossing` and `nesting` are helpers that are basically identical to
#' tidyr's `crossing` and `nesting`.
#'
#'
#'
#' @rdname f_expand
#' @export
f_expand <- function(data, ..., sort = FALSE, .by = NULL, .cols = NULL){
  group_vars <- get_groups(data, {{ .by }})
  data2 <- f_group_by(data, .by = {{ .by }}, .add = TRUE)
  if (!is.null(.cols)){
    dot_vars <- col_select_pos(data, .cols = .cols)
    frames <- vector("list", length(dot_vars))
    for (i in seq_along(dot_vars)){
      frames[[i]] <- f_distinct(data2, .cols = dot_vars[i], sort = sort)
    }
  } else {
    dots <- rlang::enquos(...)
    frames <- vector("list", length(dots))
    for (i in seq_along(dots)){
      frames[[i]] <- f_distinct(
        df_ungroup(dplyr::reframe(data2, !!!dots[i])),
        sort = sort
      )
    }
  }
  if (length(group_vars) > 0){
    anon_join <- function(x, y){
      if (length(intersect(names(x), names(y))) > length(group_vars)){
        stop("duplicate variable names supplied, please fix")
      }
      collapse_join(x, y, how = "full", on = group_vars, multiple = TRUE)
    }
    out <- Reduce(anon_join, frames)
  } else {
    if (prod(cpp_nrows(frames)) > .Machine$integer.max){
      stop("expansion results in >= 2^31 rows, please supply less data")
    }
    out <- do.call(cross_join, frames)
    # out <- Reduce(function(x, y) df_cross_join(x, y, .repair_names = FALSE), frames, simplify = FALSE)
  }
  # If just empty list
  if (length(out) == 0){
    out <- f_select(group_data(data2), .cols = group_vars)
  }
  reconstruct(data, out)
}
#' @rdname f_expand
#' @export
f_complete <- function(data, ...,  sort = FALSE,
                       .by = NULL, .cols = NULL,
                       fill = NA){
  group_vars <- get_groups(data, {{ .by }})
  expanded_df <- f_expand(data, ..., sort = FALSE,
                          .by = {{ .by }}, .cols = .cols)
  fill_na <- any(!is.na(fill))
  out <- data
  # Full-join
  if (df_nrow(expanded_df) > 0 && df_ncol(expanded_df) > 0){
    extra <- cheapr::setdiff_(
      expanded_df,
      cheapr::sset(out, j = names(expanded_df))
    )
    if (df_nrow(extra) > 0){
      extra <- df_cbind(
        extra,
        df_init(cheapr::sset(out, j = setdiff(names(out), names(expanded_df))),
                df_nrow(extra))
      )
      out <- collapse::rowbind(out, extra)
    }
    if (sort){
      out <- f_arrange(out, .cols = c(group_vars, setdiff(names(expanded_df), group_vars)))
    }
  }
  # Replace NA with fill
  if (fill_na){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      if (length(fill[[i]]) != 1){
        stop("fill values must be of length 1")
      }
      out[[fill_nms[[i]]]][cheapr::which_na(out[[fill_nms[[i]]]])] <-
        fill[[i]]
    }
  }
  out_order <- c(names(data), setdiff(names(out), names(data)))
  out <- f_select(out, .cols = out_order)
  reconstruct(data, out)
}
#' @rdname f_expand
#' @export
crossing <- function(..., sort = FALSE){
  dots <- rlang::dots_list(..., .named = TRUE)
  for (i in seq_along(dots)){
    if (is_df(dots[[i]])){
      dots[[i]] <- f_distinct(df_as_tbl(dots[[i]]), sort = sort)
    } else {
      dots[[i]] <- f_distinct(`names<-`(new_tbl(dots[[i]]), names(dots)[i]), sort = sort)
    }
  }
  do.call(cross_join, dots)
}
#' @rdname f_expand
#' @export
nesting <- function(..., sort = FALSE){
  dots <- rlang::dots_list(..., .named = TRUE)
  for (i in seq_along(dots)){
    if (is_df(dots[[i]])){
      dots[[i]] <- df_as_tbl(dots[[i]])
    } else {
      dots[[i]] <- `names<-`(new_tbl(dots[[i]]), names(dots)[i])
    }
  }
  f_distinct(do.call(df_cbind, dots), sort = sort)
}
# f_crossing <- function(..., sort = FALSE){
#   # dot_nms <- unlist(
#   #   lapply(
#   #     substitute(alist(...))[-1L], deparse
#   #   ), recursive = FALSE,
#   #   use.names = TRUE
#   # )
#   # dots <- list(...)
#   # if (is.null(names(dots))){
#   #   names(dots) <- dot_nms
#   # }
#   # names(dots)[!nzchar(names(dot_nms))] <- dot_nms[!nzchar(names(dot_nms))]
#
#   dots <- rlang::dots_list(..., .named = TRUE)
#   # dots <- lapply(dots, sort_unique, sort)
#   for (i in seq_along(dots)){
#     if (is_df(dots[[i]])){
#       dots[[i]] <- f_distinct(df_as_tbl(dots[[i]]), sort = sort)
#     } else {
#       dots[[i]] <- f_distinct(`names<-`(new_tbl(dots[[i]]), names(dots)[i]), sort = sort)
#     }
#   }
#   do.call(cross_join, dots)
#   # out <- do.call(cross_join, dots)
#   # names(out) <- unique_name_repair(names(dots))
#   # out
#
#   # do.call(cross_join, rlang::dots_list(..., .named = TRUE))
# }
# f_nesting <- function(..., sort = FALSE){
#   # dot_nms <- unlist(
#   #   lapply(
#   #     substitute(alist(...))[-1L], deparse
#   #   ), recursive = FALSE,
#   #   use.names = TRUE
#   # )
#   # dots <- list(...)
#   # if (is.null(names(dots))){
#   #   names(dots) <- dot_nms
#   # }
#   # names(dots)[!nzchar(names(dot_nms))] <- dot_nms[!nzchar(names(dot_nms))]
#   # names(dots) <- unique_name_repair(names(dots))
#   dots <- rlang::dots_list(..., .named = TRUE)
#   for (i in seq_along(dots)){
#     if (is_df(dots[[i]])){
#       dots[[i]] <- df_as_tbl(dots[[i]])
#     } else {
#       dots[[i]] <- `names<-`(new_tbl(dots[[i]]), names(dots)[i])
#     }
#   }
#   # dots <- lapply(dots, function(x) if (is_df(x)) df_as_tbl(x) else new_tbl(x = x))
#   # out <- list_as_tbl(dots)
#   out <- do.call(df_cbind, dots)
#   # names(out) <- unique_name_repair(names(dots))
#   f_distinct(out, sort = sort)
# }

sort_unique <- function(x, sort = FALSE){
  if (is_df(x)){
    f_distinct(x, sort = sort)
  } else {
    collapse::funique(x, sort = sort)
  }
}
