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

  all_groups <- get_groups(.data, .by = {{ .by }})
  GRP <- df_to_GRP(.data, all_groups, order = .order)
  quos <- fastplyr_quos(..., .drop_null = TRUE,
                        .unpack_default = TRUE,
                        .optimise = should_optimise(GRP),
                        .groups = GRP)
  group_keys <- GRP_groups(GRP)

  if (length(quos) == 0){
    return(cheapr::reconstruct(GRP_groups(GRP), cpp_ungroup(.data)))
  }
  results <- eval_all_tidy(quos, recycle = TRUE)
  groups <- results[["groups"]]
  results <- results[["results"]]
  n_group_vars <- length(GRP_group_vars(GRP))
  if (n_group_vars == 0){
    groups <- cheapr::new_df(.nrows = cheapr::vector_length(results[[1L]]))
  } else {
    groups <- list_as_df(groups[[1L]])
  }
  out <- df_add_cols(groups, results)
  out <- cheapr::sset_col(out, !duplicated(names(out), fromLast = TRUE))
  cheapr::reconstruct(out, cpp_ungroup(.data))
}
