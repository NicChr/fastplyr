#' A faster `reframe()` with per-group optimisations
#'
#' @inheritParams f_summarise
#'
#' @inheritSection f_summarise Details
#'
#' @returns
#' A data frame of specified results.
#'
#' @export
f_reframe <- function(.data, ..., .by = NULL, .order = df_group_by_order_default(.data)){

  if (rlang::quo_is_null(rlang::enquo(.by))){
    data <- .data
  } else {
    data <- f_group_by(.data, .by = {{ .by }}, .add = TRUE, .order = .order)
  }
  if (length(group_vars(data)) == 0 || df_nrow(group_keys(data)) < 1e04){
    .optimise <- FALSE
  } else {
    .optimise <- TRUE
  }
  quos <- fastplyr_quos(..., .data = data, .drop_null = TRUE, .unpack_default = TRUE,
                        .optimise = .optimise)

  if (length(quos) == 0){
    return(cheapr::reconstruct(group_keys(data), cpp_ungroup(.data)))
  }
  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    out <- dplyr::reframe(data, ...)
  } else {
    results <- eval_all_tidy(quos, recycle = TRUE)
    groups <- results[["groups"]]
    results <- results[["results"]]
    n_group_vars <- length(group_vars(data))
    if (n_group_vars == 0){
      groups <- cheapr::new_df(.nrows = cheapr::vector_length(results[[1L]]))
    } else {
      groups <- list_as_df(groups[[1L]])
    }
    out <- df_add_cols(groups, results)
    out <- cheapr::sset_col(out, !duplicated(names(out), fromLast = TRUE))
  }
  cheapr::reconstruct(out, cpp_ungroup(.data))
}
