#' Summarise each group down to one row
#'
#' @description Like `dplyr::summarise()` but with some internal optimisations
#' for common statistical functions.
#'
#' @param data A data frame.
#' @param ... Name-value pairs of summary functions. Expressions with
#' `across()` are also accepted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .order Should the groups be returned in sorted order?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#' @param .optimise (Optionally) turn off optimisations for common statistical
#' functions by setting to `FALSE`. Default is `TRUE` which uses optimisations.
#'
#' @seealso [tidy_quantiles]
#'
#' @returns
#' An un-grouped data frame of summaries by group.
#'
#' @details
#' `f_summarise` behaves mostly like `dplyr::summarise` except that expressions
#' supplied to `...` are evaluated independently.
#'
#' ### Optimised statistical functions
#'
#' Some functions are internally optimised using 'collapse'
#' fast statistical functions. This makes execution on many groups very fast.
#'
#' For fast quantiles (percentiles) by group, see [tidy_quantiles]
#'
#'
#' List of currently optimised functions and their equivalent
#' 'collapse' function
#'
#' `base::sum` -> `collapse::fsum` \cr
#' `base::prod` -> `collapse::fprod` \cr
#' `base::min` -> `collapse::fmin` \cr
#' `base::max` -> `collapse::fmax` \cr
#' `stats::mean` -> `collapse::fmean` \cr
#' `stats::median` -> `collapse::fmedian` \cr
#' `stats::sd` -> `collapse::fsd` \cr
#' `stats::var` -> `collapse::fvar` \cr
#' `dplyr::first` -> `collapse::ffirst` \cr
#' `dplyr::last` -> `collapse::flast` \cr
#' `dplyr::n_distinct` -> `collapse::fndistinct` \cr
#'
#' @examples
#' library(fastplyr)
#' library(nycflights13)
#'
#' # Number of flights per month, including first and last day
#' flights %>%
#'   f_group_by(year, month) %>%
#'   f_summarise(first_day = first(day),
#'               last_day = last(day),
#'               num_flights = n())
#'
#' ## Fast mean summary using `across()`
#'
#' flights %>%
#'   f_summarise(
#'     across(where(is.double), mean),
#'     .by = tailnum
#'   )
#'
#' # To ignore or keep NAs, use collapse::set_collapse(na.rm)
#' collapse::set_collapse(na.rm = FALSE)
#' flights %>%
#'   f_summarise(
#'     across(where(is.double), mean),
#'     .by = origin
#'   )
#' collapse::set_collapse(na.rm = TRUE)
#' @export
f_summarise <- function(.data, ..., .by = NULL, .order = df_group_by_order_default(.data)){
  if (rlang::quo_is_null(rlang::enquo(.by))){
    data <- .data
  } else {
    data <- f_group_by(.data, .by = {{ .by }}, .add = TRUE, .order = .order)
  }
  group_keys <- group_keys(data)
  if (df_nrow(.data) == 0){
    group_keys <- cheapr::sset_df(group_keys, 0L)
  }
  if (length(group_vars(data)) == 0 || df_nrow(group_keys) < 1e04){
    .optimise <- FALSE
  } else {
    .optimise <- TRUE
  }
  quos <- fastplyr_quos(..., .data = data, .drop_null = TRUE,
                        .unpack_default = TRUE, .optimise = .optimise)

  if (length(quos) == 0){
    return(cheapr::reconstruct(group_keys, cpp_ungroup(.data)))
  }
  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    out <- dplyr::summarise(data, ...)
  } else {
    ## The `recycle` argument won't have a visible effect
    # on the final result, but it's faster to
    # set as TRUE as it means the group keys only get recycled once internally
    # and recycling between results shouldn't happen as they should all be
    # of equal length
    results <- eval_all_tidy(quos, recycle = TRUE)
    groups <- results[["groups"]]
    results <- results[["results"]]

    result_sizes <- cheapr::list_lengths(results)
    # if (any(result_sizes != min(df_nrow(data), df_nrow(group_keys)))){
    if (any(result_sizes != df_nrow(group_keys))){
      cli::cli_abort(c("All expressions should return results of length 1 per-group",
                       "Use {.run f_reframe()} instead"))
    }
    out <- df_add_cols(group_keys, results)
    out <- cheapr::sset_col(out, !duplicated(names(out), fromLast = TRUE))
  }
  cheapr::reconstruct(out, cpp_ungroup(.data))
}
#' @export
f_summarize <- f_summarise

across_col_names <- function (.cols = NULL, .fns = NULL, .names = NULL){
  fns_null <- is.null(.fns)
  nms_null <- is.null(.names)
  n_fns <- length(.fns)
  n_cols <- length(.cols)

  if (fns_null && nms_null){
    out <- as.character(.cols)
  } else if (nms_null && n_fns == 1L) {
    out <- .cols
  } else if (nms_null && n_cols == 1L) {
    out <- .fns
    out <- cheapr::name_repair(out, empty_sep = paste0(.cols, "_"), dup_sep = "_")
  } else {
    .fns <- cheapr::name_repair(.fns, empty_sep = "", dup_sep = "")
    out <- character(n_cols * n_fns)
    init <- 0L
    if (nms_null) {
      for (.col in .cols) {
        out[seq_len(n_fns) + init] <- paste0(.col, "_", .fns)
        init <- init + n_fns
      }
    } else {
      .fn <- .fns
      for (.col in .cols) {
        out[seq_len(n_fns) + init] <- stringr::str_glue(.names)
        init <- init + n_fns
      }
    }
  }
  out
}

is_anonymous_function <- function(x){
  is.function(x) && identical(names(attributes(x)), "srcref")
}


