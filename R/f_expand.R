#' Fast versions of `tidyr::expand()` and `tidyr::complete()`.
#'
#' @param data A data frame
#' @param ... Variables to expand.
#' @param fill A named list containing value-name pairs
#' to fill the named implicit missing values.
#' @param .sort Logical. If `TRUE` expanded/completed variables are sorted.
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
f_expand <- function(data, ..., .sort = FALSE,
                     .by = NULL, .cols = NULL){
  check_cols(dots_length(...), .cols = .cols)
  group_vars <- get_groups(data, {{ .by }})

  if (length(group_vars) == 0L){
    GRP <- NULL
  } else {
    GRP <- df_to_GRP(data, group_vars, order = .sort)
  }

  # If the user is simply selecting cols then we can use an optimised method
  if (is.null(.cols)){
    dots <- fastplyr_quos(..., .data = data, .groups = GRP)
    dot_labels <- quo_labels(dots)
    if (all(dot_labels %in% names(data)) && !any(names(dots) %in% names(data))){
      .cols <- dot_labels
    }
  }
  # Optimised method when just selecting cols
  if (!is.null(.cols)){
    data2 <- cpp_ungroup(data)
    dot_vars <- col_select_names(data2, .cols = .cols)
    frames <- cheapr::new_list(length(dot_vars))
    for (i in seq_along(dot_vars)){
      frames[[i]] <- sort_unique(
        f_select(data2, .cols = c(group_vars, dot_vars[i])),
        sort = .sort
      )
    }
  } else {
    data2 <- data
    frames <- eval_all_tidy(data2, dots, recycle = FALSE)
    frames <- purrr::map2(
      frames[[1L]], cpp_as_list_of_frames(frames[[2L]]),
      \(x, y) sort_unique(cheapr::col_c(x, y), .sort)
    )
  }
  if (length(group_vars) > 0){
    anon_join <- function(x, y){
      f_full_join(x, y, by = group_vars, suffix = c("", ".fastplyr.suffix"),
                  keep = FALSE)
    }
    out <- Reduce(anon_join, frames)

    ## Here we remove the distinct join suffix and use cheapr::name_repair()
    ## for duplicate col names

    which_suffix_names <- which(grepl(".fastplyr.suffix", names(out), fixed = TRUE))
    names(out)[which_suffix_names] <-
      gsub(".fastplyr.suffix", "", names(out)[which_suffix_names],
           fixed = TRUE)
  } else {
    if (prod(cpp_frame_dims(frames, FALSE, FALSE)[[1L]]) > .Machine$integer.max){
      cli::cli_abort("expansion results in >= 2^31 rows, please supply less data")
    }
    out <- Reduce(cross_join2, frames)

    # Alternative
    # out <- do.call(cross_join, frames)
  }
  names(out) <- cheapr::name_repair(names(out))
  # If just empty list
  if (length(frames) == 0){
    out <- f_distinct(data2, .cols = group_vars, .order = .sort)
  }
  cheapr::rebuild(out, data)
}
#' @rdname f_expand
#' @export
f_complete <- function(data, ...,
                       .sort = FALSE,
                       .by = NULL, .cols = NULL,
                       fill = NA){
  group_vars <- get_groups(data, {{ .by }})
  expanded_df <- f_expand(data, ..., .sort = FALSE,
                          .by = {{ .by }}, .cols = .cols)
  fill_na <- any(!is.na(fill))
  out <- data

  if (df_nrow(expanded_df) > 0 && df_ncol(expanded_df) > 0){
    # out <- f_full_join(out, expanded_df, by = names(expanded_df), sort = .sort)

    # Alternative method using essentially setdiff() + rbind()
    extra <- f_anti_join(expanded_df, f_select(out, .cols = names(expanded_df)))
    if (df_nrow(extra) > 0){
      out <- f_bind_rows(out, extra)
    }
  }
  if (.sort){
    out <- f_arrange(out, .cols = names(expanded_df))
  }
  # Replace NA with fill
  if (fill_na){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      if (length(fill[[i]]) != 1){
        cli::cli_abort("fill values must be of length 1")
      }
      out[[fill_nms[[i]]]][cheapr::which_na(out[[fill_nms[[i]]]])] <-
        fill[[i]]
    }
  }
  out_order <- c(names(data), vec_setdiff(names(out), names(data)))
  out <- f_select(out, .cols = out_order)
  cheapr::rebuild(out, data)
}
#' @rdname f_expand
#' @export
crossing <- function(..., .sort = FALSE){
  dots <- list_tidy(..., .named = TRUE, .keep_null = FALSE)
  dots <- lapply(dots, sort_unique, .sort)
  df_as_tbl(do.call(cross_join, dots))
}
#' @rdname f_expand
#' @export
nesting <- function(..., .sort = FALSE){
  df_as_tbl(
    sort_unique(
      cheapr::col_c(
        .args = list_tidy(..., .named = TRUE, .keep_null = FALSE),
        .recycle = TRUE, .name_repair = TRUE
      ), sort = .sort
    )
  )
}
