#' Fast grouped sample quantiles
#'
#' @param data A data frame.
#' @param ... `<data-masking>` Variables to calculate quantiles for.
#' @param probs `numeric(n)` - Quantile probabilities.
#' @param type `integer(1)` - Quantile type, see `?collapse::fquantile`
#' @param pivot `character(1)` - Pivot result wide or long? Default is "wide".
#' @param na.rm `logical(1)` Should `NA` values be ignored? Default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .drop_groups `lifecycle::badge("deprecated")`
#' @param .order Should the groups be returned in sorted order?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#'
#' @returns
#' A data frame of sample quantiles.
#'
#' @examples
#' library(fastplyr)
#' library(dplyr)
#' groups <- 1 * 2^(0:10)
#'
#' # Normal distributed samples by group using the group value as the mean
#' # and sqrt(groups) as the sd
#'
#' samples <- tibble(groups) %>%
#'   reframe(x = rnorm(100, mean = groups, sd = sqrt(groups)), .by = groups) %>%
#'   f_group_by(groups)
#'
#' # Fast means and quantiles by group
#'
#' quantiles <- samples %>%
#'   tidy_quantiles(x, pivot = "wide")
#'
#' means <- samples %>%
#'   f_summarise(mean = mean(x))
#'
#' means %>%
#'   f_left_join(quantiles)
#' @export
tidy_quantiles <- function(data, ..., probs = seq(0, 1, 0.25),
                           type = 7, pivot = c("long", "wide"),
                           na.rm = TRUE,
                           .by = NULL, .cols = NULL,
                           .order = group_by_order_default(data),
                           .drop_groups = deprecated()){
  pivot <- rlang::arg_match(pivot)
  wide <- pivot == "wide"
  group_info <- tidy_dots_info(
    data, ..., .by = {{ .by }}, .cols = .cols, .order = .order
  )
  data2 <- group_info[["data"]]
  group_vars <- group_info[["all_groups"]]
  dot_vars <- group_info[["new_cols"]]
  groups <- group_info[["GRP"]]

  # Constructing quantile info
  quant_probs <- as.double(probs)
  n_probs <- length(quant_probs)

  sorted_probs <- isTRUE(!is.unsorted(quant_probs))

  quant_ties <- paste0("q", type)
  q_prcnts <- quant_probs * 100
  quant_nms <- paste0(rep_len("p", length(quant_probs)), round(q_prcnts, 10))
  quant_categories <- structure(
    strip_attrs(collapse::group(q_prcnts)),
    levels = collapse::funique(quant_nms), class = "factor"
  )

  if (is.null(groups)){
    n_groups <- 1L
    group_starts <- min(1L, df_nrow(data2))
    group_ids <- integer(df_nrow(data2))
  } else {
    n_groups <- GRP_n_groups(groups)
    group_starts <- GRP_starts(groups)
    group_ids <- GRP_group_id(groups)
  }


  data2 <- f_select(data2, .cols = c(group_vars, dot_vars))

  ## Handle edge-cases

  if (wide && length(dot_vars) == 0){
    empty_quant_df <-
      cheapr::new_list(
        length(quant_nms),
        default = numeric()
      )
    names(empty_quant_df) <- quant_nms
    empty_quant_df <- as.data.frame(empty_quant_df)
    out <- f_bind_cols(cheapr::sset_col(data2, group_vars), empty_quant_df)
    return(cheapr::reconstruct(out, data))
  }
  if (df_nrow(data) == 0L || n_probs == 0L){
    if (wide){
      prob_df <- matrix(
        numeric(),
        ncol = (n_probs * length(dot_vars)),
        nrow = 0
      )
      if (length(dot_vars) == 1){
        colnames(prob_df) <- quant_nms
      } else {
        colnames(prob_df) <- paste(rep(dot_vars, each = n_probs),
                                   quant_nms, sep = "_")
      }
      prob_df <- as.data.frame(prob_df)
      out <- f_bind_cols(cheapr::sset_col(data2, group_vars), prob_df)
    } else {
      out <- f_bind_cols(
        cheapr::sset_col(data2, group_vars),
        cheapr::new_df(
          .quantile = quant_categories[0]
        ),
        cheapr::sset_col(data2, dot_vars)
      )
    }
    return(cheapr::reconstruct(out, data))
  }

  ## Make sure double vectors are atomic doubles

  for (.col in dot_vars) {
    if (!is.integer(data2[[.col]])){
      data2[[.col]] <- as.numeric(data2[[.col]])
    }
  }

  ## If probs are unsorted then we cant use fquantile() directly
  if (length(group_vars) == 0 && sorted_probs){

    ## Ungrouped method (use `fquantile()` here)

    out <- list(.quantile = quant_categories)
    for (.col in dot_vars){
      out[[.col]] <-
        as.double(
          collapse::fquantile(
            data2[[.col]], probs = quant_probs, na.rm = na.rm,
            names = FALSE, type = type,
          )
        )
    }
    out <- cheapr::list_as_df(out)
    if (wide){
      out <- df_add_col(out, ".temp.fastplyr.group.id", 0L)
      out <- collapse::pivot(
        out, how = "wider", values = dot_vars,
        names = ".quantile", sort = FALSE
      )
      out <- df_rm_cols(out, ".temp.fastplyr.group.id")
    }
  } else if (wide){

    # Grouped method for pivot == "wide"

    out <- cheapr::sset_row(cheapr::sset_col(data2, group_vars), group_starts)
    # Allocate enough space
    out <- cheapr::list_combine(out, cheapr::new_list(length(dot_vars) * n_probs))
    if (length(dot_vars) == 1){
      names(out) <- c(group_vars, quant_nms)
    } else {
      names(out) <- c(group_vars, paste(rep(dot_vars, each = n_probs),
                                        quant_nms, sep = "_"))
    }
    k <- 1L + length(group_vars)
    for (.col in dot_vars) {

      # Pre-calculate the quantile order
      # This makes repetitive calls much faster
      o <- radixorderv2(
        cheapr::new_df(
          g1 = group_ids,
          g2 = data2[[.col]]
        )
      )
      for (p in quant_probs) {
        if (p == 0) {
          sample_quantiles <-
            as.double(
              collapse::fmin(
                data2[[.col]], g = groups, na.rm = na.rm, use.g.names = FALSE
              )
            )
        } else if (p == 1) {
          sample_quantiles <-
            as.double(
              collapse::fmax(
                data2[[.col]], g = groups, na.rm = na.rm, use.g.names = FALSE
              )
            )
        } else if (p > 0 && p < 1) {
          sample_quantiles <-
            as.double(
              collapse::fnth(
                data2[[.col]], n = p, g = groups, na.rm = na.rm,
                use.g.names = FALSE, ties = quant_ties,
                o = o, check.o = FALSE
              )
            )
        }
        out[[k]] <- sample_quantiles
        k <- k + 1L
      }
    }
    out <- cheapr::list_as_df(out)
  } else {

    # Grouped method for pivot == "long"


    ## Shaping the data
    ## We want it sorted by group + quantile

    out <- cheapr::sset_row(data2, group_starts)
    out <- cheapr::cheapr_rep_each(out, n_probs)
    out[[".quantile"]] <- rep(quant_categories, df_nrow(out) / n_probs)
    out <- cheapr::sset_col(out, c(group_vars, ".quantile", dot_vars))

    ## We make sure all output quantile cols are double vectors
    ## Because later we use a low-level function for replacing
    ## values by reference

    if (length(dot_vars) > 0) {
      out <- f_mutate(
        out, across(all_of(dot_vars), as.double)
      )
    }
    quant_starts <- ( n_probs * (seq_len(n_groups) - 1L) ) + 1L

    for (.col in dot_vars) {
      k <- 0L

      # Pre-calculate the quantile order
      # This makes repetitive calls much faster
      o <- radixorderv2(
        cheapr::new_df(
          g1 = group_ids,
          g2 = data2[[.col]]
        )
      )

      # Replace values in-place
      # this is fine because out is a fresh data because we called `sset_df()`

      for (p in quant_probs) {
        p_seq <- quant_starts + k
        k <- k + 1L
        if (p == 0) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fmin(
                data2[[.col]], g = groups, na.rm = na.rm,
                use.g.names = FALSE
              )
            )
          )
        } else if (p == 1) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fmax(
                data2[[.col]], g = groups, na.rm = na.rm,
                use.g.names = FALSE
              )
            )
          )
        } else if (p > 0 && p < 1) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fnth(
                data2[[.col]], n = p, g = groups, na.rm = na.rm,
                use.g.names = FALSE, ties = quant_ties,
                o = o, check.o = FALSE
              )
            )
          )
        }
      }
    }
  }

  if (wide){
    cheapr::reconstruct(out, cpp_ungroup(data))
  } else {
    cheapr::reconstruct(out, data)
  }
}
