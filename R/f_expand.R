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
#' @rdname f_expand
#' @export
f_expand <- function(data, ..., sort = FALSE, .by = NULL, .cols = NULL){
  check_cols(dots_length(...), .cols = .cols)
  group_vars <- get_groups(data, {{ .by }})
  if (!is.null(.cols)){
    data2 <- df_ungroup(data)
    dot_vars <- col_select_names(data2, .cols = .cols)
    frames <- vector("list", length(dot_vars))
    for (i in seq_along(dot_vars)){
      frames[[i]] <- f_distinct(data2, .cols = c(group_vars, dot_vars[i]), sort = sort)
    }
  } else {
    data2 <- f_group_by(data, .by = {{ .by }}, .add = TRUE)
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
      f_full_join(x, y, by = group_vars)
    }
    out <- Reduce(anon_join, frames)
  } else {
    if (prod(cpp_nrows(frames)) > .Machine$integer.max){
      stop("expansion results in >= 2^31 rows, please supply less data")
    }
    df_cj <- function(x, y){
      df_cross_join(x, y, .repair_names = FALSE)
    }
    out <- Reduce(df_cj, frames)
    names(out) <- unique_name_repair(names(out))

    # Alternative
    # out <- do.call(cross_join, frames)
  }
  # If just empty list
  if (length(out) == 0){
    out <- f_distinct(data2, .cols = group_vars, sort = sort)
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
      extra <- f_bind_cols(
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
  dots <- named_dots(...)
  for (i in seq_along(dots)){
    if (!is_df(dots[[i]])){
      dots[[i]] <- sort_unique(`names<-`(new_df(dots[[i]]), names(dots)[i]), sort = sort)
    }
  }
  df_as_tbl(do.call(cross_join, dots))
}
#' @rdname f_expand
#' @export
nesting <- function(..., sort = FALSE){
  dots <- named_dots(...)
  for (i in seq_along(dots)){
    if (!is_df(dots[[i]])){
      dots[[i]] <- `names<-`(new_df(dots[[i]]), names(dots)[i])
    }
  }
  df_as_tbl(sort_unique(do.call(f_bind_cols, dots), sort = sort))
}

# Very fast unique function
sort_unique <- function(x, sort = FALSE){
  if (sort){
    o <- radixorderv2(x, starts = TRUE)
    starts <- o[attr(o, "starts")]
    subset <- !(length(starts) == NROW(x) && isTRUE(attr(o, "sorted")))
  } else {
    starts <- attr(group3(x, starts = TRUE), "starts")
    subset <- length(starts) != NROW(x)
  }
  if (subset){
    cheapr::sset(x, starts)
  } else {
    x
  }
}
