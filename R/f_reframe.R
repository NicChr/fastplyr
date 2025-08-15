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
f_reframe <- function(.data, ..., .by = NULL, .order = group_by_order_default(.data)){

  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0L){
    GRP <- NULL
    group_keys <- f_group_keys(.data)
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
    group_keys <- GRP_groups(GRP)
  }
  quos <- fastplyr_quos(..., .data = .data, .drop_null = TRUE,
                        .unpack_default = TRUE,
                        .optimise = should_optimise(GRP),
                        .groups = GRP)

  if (length(quos) == 0){
    return(cheapr::rebuild(group_keys, f_ungroup(.data)))
  }
  results <- eval_all_tidy(.data, quos, recycle = TRUE)
  groups <- results[["groups"]]
  results <- results[["results"]]
  n_group_vars <- length(GRP_group_vars(GRP))
  if (n_group_vars == 0){
    groups <- cheapr::new_df(.nrows = vector_length(results[[1L]]))
  } else {
    groups <- cheapr::list_as_df(groups[[1L]])
  }
  out <- cheapr::df_modify(groups, results)
  out <- cheapr::sset_col(out, !duplicated(names(out), fromLast = TRUE))
  cheapr::rebuild(out, f_ungroup(.data))
}
