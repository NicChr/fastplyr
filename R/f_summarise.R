#' Summarise each group down to one row
#'
#' @description Like `dplyr::summarise()` but with some internal optimisations
#' for common statistical functions.
#'
#' @param .data A data frame.
#' @param ... Name-value pairs of summary functions. Expressions with
#' `across()` are also accepted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .order Should the groups be returned in sorted order?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#'
#' @seealso [tidy_quantiles]
#'
#' @returns
#' An un-grouped data frame of summaries by group.
#'
#' @section Details:
#'
#' fastplyr data-masking functions like `f_mutate` and `f_summarise` operate
#' very similarly to their dplyr counterparts but with some crucial
#' differences.
#' Optimisations for by-group operations kick in for
#' common statistical functions which are detailed below.
#' A message will be printed which one can disable
#' by running `options(fastplyr.inform = FALSE)`.
#' When this happens, the expressions which become optimised no longer
#' obey data-masking rules pertaining to sequential and dependent expression
#' execution.
#' For example,
#' the pseudo code
#'  `f_summarise(data, mean = mean(x), mean2 = round(mean), .by = g)`
#' when optimised will not work because the named col `mean` will not be visible
#' in later expressions.
#'
#' One can disable fastplyr optimisations
#' globally by running `options(fastplyr.optimise = F)`.
#'
#' ### Optimised statistical functions
#'
#' Some functions are internally optimised using 'collapse'
#' fast statistical functions. This makes execution on many groups very fast.
#'
#' For fast quantiles (percentiles) by group, see [tidy_quantiles]
#'
#'
#' List of currently optimised functions
#'
#' `dplyr::n` -> <custom_expression> \cr
#' `dplyr::row_number` -> <custom_expression> (only for `f_mutate`) \cr
#' `dplyr::cur_group` -> <custom_expression> \cr
#' `dplyr::cur_group_id` -> <custom_expression> \cr
#' `dplyr::cur_group_rows` -> <custom_expression> (only for `f_mutate`) \cr
#' `dplyr::lag` -> <custom_expression> (only for `f_mutate`) \cr
#' `dplyr::lead` -> <custom_expression> (only for `f_mutate`) \cr
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
#' library(dplyr)
#' options(fastplyr.inform = FALSE)
#' # Number of flights per month, including first and last day
#' flights |>
#'   f_group_by(year, month) |>
#'   f_summarise(first_day = first(day),
#'               last_day = last(day),
#'               num_flights = n())
#'
#' ## Fast mean summary using `across()`
#'
#' flights |>
#'   f_summarise(
#'     across(where(is.numeric), mean),
#'     .by = tailnum
#'   )
#'
#' flights |>
#'   f_group_by(.cols = "tailnum") |>
#'   f_summarise(
#'     across(where(is.numeric), mean)
#'   )
#' @rdname f_summarise
#' @export
f_summarise <- function(.data, ..., .by = NULL, .order = group_by_order_default(.data)){
  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0L){
    GRP <- NULL
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
  }
  quos <- fastplyr_quos(..., .data = .data, .groups = GRP,
                        .drop_null = TRUE,
                        .unpack_default = TRUE,
                        .optimise = should_optimise(GRP))

  results <- eval_summarise(.data, quos)
  groups <- results[["groups"]]
  out <- cheapr::df_modify(groups, results[["results"]])
  out <- cheapr::sset_col(out, !duplicated(names(out), fromLast = TRUE))

  cheapr::rebuild(out, cpp_ungroup(.data))
}
#' @rdname f_summarise
#' @export
f_summarize <- f_summarise

across_col_names <- function (.cols = NULL, .fns = NULL, .names = NULL){
  fns_null <- is.null(.fns)
  nms_null <- is.null(.names)

  if (fns_null && !nms_null){
    .fns <- ""
    fns_null <- FALSE
  }

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
    .fns <- cheapr::name_repair(.fns %||% "", empty_sep = "", dup_sep = "")
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

