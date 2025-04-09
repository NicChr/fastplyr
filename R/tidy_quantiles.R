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
#' @param .drop_groups `logical(1)` Should groups be dropped after calculation?
#' Default is `TRUE`.
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
                           .order = df_group_by_order_default(data),
                           .drop_groups = TRUE){
  pivot <- rlang::arg_match(pivot)
  wide <- pivot == "wide"
  group_info <- tidy_group_info(
    data, ..., .by = {{ .by }}, .cols = .cols, ungroup = TRUE
  )
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]

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

  data2 <- group_info[["data"]]
  groups <- df_to_GRP(data2, .cols = group_vars, order = .order)
  n_groups <- GRP_n_groups(groups)
  group_starts <- GRP_starts(groups)
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
    out <- f_bind_cols(f_select(data2, .cols = group_vars), empty_quant_df)
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
      out <- f_bind_cols(f_select(data2, .cols = group_vars), prob_df)
    } else {
      out <- f_bind_cols(
        f_select(data2, .cols = group_vars),
        cheapr::new_df(
          .quantile = quant_categories[0]
        ),
        f_select(data2, .cols = dot_vars)
      )
    }
    return(cheapr::reconstruct(out, data))
  }

  ### Do this because if groups is a GRP then collapse::fnth allocates
  ### more memory than needed in the special case of <= 1 groups

  if (n_groups <= 1){
    collapse_groups <- NULL
  } else {
    collapse_groups <- groups
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
    out <- list_as_df(out)
    if (wide){
      out[[".temp.fastplyr.group.id"]] <- 0L
      out <- collapse::pivot(
        out, how = "wider", values = dot_vars,
        names = ".quantile", sort = FALSE
      )
      out[[".temp.fastplyr.group.id"]] <- NULL
    }
  } else if (wide){

    # Grouped method for pivot == "wide"

    out <- cheapr::sset_df(f_select(data2, .cols = group_vars), group_starts)
    # Allocate enough space
    out <- c(as.list(out), vector("list", length(dot_vars) * n_probs))
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
          g1 = GRP_group_id(groups),
          g2 = data2[[.col]]
        )
      )
      for (p in quant_probs) {
        if (p == 0) {
          sample_quantiles <-
            as.double(
              collapse::fmin(
                data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
              )
            )
        } else if (p == 1) {
          sample_quantiles <-
            as.double(
              collapse::fmax(
                data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
              )
            )
        } else if (p > 0 && p < 1) {
          sample_quantiles <-
            as.double(
              collapse::fnth(
                data2[[.col]], n = p, g = collapse_groups, na.rm = na.rm,
                use.g.names = FALSE, ties = quant_ties,
                o = o, check.o = FALSE
              )
            )
        }
        out[[k]] <- sample_quantiles
        k <- k + 1L
      }
    }
    out <- list_as_df(out)
  } else {

    # Grouped method for pivot == "long"


    ## Shaping the data
    ## We want it sorted by group + quantile

    out <- cheapr::sset_df(data2, group_starts)
    out <- cheapr::cheapr_rep_each(out, n_probs)
    out[[".quantile"]] <- rep(quant_categories, df_nrow(out) / n_probs)
    out <- f_select(out, .cols = c(group_vars, ".quantile", dot_vars))

    ## We make sure all output quantile cols are double vectors
    ## Because later we use a low-level function for replacing
    ## values by reference

    if (length(dot_vars) > 0) {
      out <- dplyr::mutate(
        out, dplyr::across(dplyr::all_of(dot_vars), as.double)
      )
    }
    quant_starts <- ( n_probs * (seq_len(n_groups) - 1L) ) + 1L

    for (.col in dot_vars) {
      k <- 0L

      # Pre-calculate the quantile order
      # This makes repetitive calls much faster
      o <- radixorderv2(
        cheapr::new_df(
          g1 = GRP_group_id(groups),
          g2 = data2[[.col]]
        )
      )

      for (p in quant_probs) {
        p_seq <- quant_starts + k
        k <- k + 1L
        if (p == 0) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fmin(
                data2[[.col]], g = collapse_groups, na.rm = na.rm,
                use.g.names = FALSE
              )
            )
          )
        } else if (p == 1) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fmax(
                data2[[.col]], g = collapse_groups, na.rm = na.rm,
                use.g.names = FALSE
              )
            )
          )
        } else if (p > 0 && p < 1) {
          cpp_loc_set_replace(
            out[[.col]], p_seq,
            as.double(
              collapse::fnth(
                data2[[.col]], n = p, g = collapse_groups, na.rm = na.rm,
                use.g.names = FALSE, ties = quant_ties,
                o = o, check.o = FALSE
              )
            )
          )
        }
      }
    }
  }

  if (.drop_groups){
    cheapr::reconstruct(out, cpp_ungroup(data))
  } else {
    cheapr::reconstruct(out, data)
  }
}
