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
#' @rdname f_expand
#' @export
f_expand <- function(data, ..., sort = FALSE, .by = NULL, .cols = NULL){
  group_vars <- get_groups(data, {{ .by }})
  data2 <- f_group_by(data, .by = {{ .by }}, .add = TRUE)
  if (!is.null(.cols)){
    dot_vars <- col_select_pos(data, .cols = .cols)
    frames <- vector("list", length(dot_vars))
    for (i in seq_along(dot_vars)){
      frames[[i]] <- f_distinct(data2, .cols = dot_vars[i])
    }
  } else {
    dots <- rlang::enquos(...)
    frames <- vector("list", length(dots))
    for (i in seq_along(dots)){
      frames[[i]] <- f_distinct(df_ungroup(dplyr::reframe(data2, !!!dots[i])))
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
  if (sort){
    out <- f_arrange_all(out)
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

