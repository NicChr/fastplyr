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
#' samples <- tibble(groups) |>
#'   reframe(x = rnorm(100, mean = groups, sd = sqrt(groups)), .by = groups) |>
#'   f_group_by(groups)
#'
#' # Fast means and quantiles by group
#'
#' quantiles <- samples |>
#'   tidy_quantiles(x)
#'
#' means <- samples |>
#'   f_summarise(mean = mean(x))
#'
#' means |>
#'   f_left_join(quantiles)
#' @export
tidy_quantiles <- function(data, ..., probs = seq(0, 1, 0.25),
                          type = 7, pivot = c("wide", "long"),
                          na.rm = TRUE,
                          .by = NULL, .cols = NULL,
                          .drop_groups = TRUE){
  pivot <- rlang::arg_match(pivot)
  wide <- pivot == "wide"
  group_info <- tidy_group_info(
    data, ..., .by = {{ .by }}, .cols = .cols, ungroup = TRUE
  )
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]

  # Constructing quantile info
  quantile_probs <- probs
  quantile_ties <- paste0("q", type)
  q_prcnts <- quantile_probs * 100
  quantile_nms <- paste0(rep_len("p", length(quantile_probs)),
                         q_prcnts)
  quantile_out_nms <- structure(strip_attrs(collapse::group(q_prcnts)),
                                levels = collapse::funique(quantile_nms), class = "factor")

  data2 <- group_info[["data"]]
  groups <- df_to_GRP(data2, .cols = group_vars, order = df_group_by_order_default(data))
  n_groups <- GRP_n_groups(groups)
  group_starts <- GRP_starts(groups)
  data2 <- f_select(data2, .cols = c(group_vars, dot_vars))

  # Some special cases

  if (wide && length(dot_vars) == 0){
    empty_quantile_df <-
      cheapr::new_list(
        length(quantile_nms),
        default = numeric()
      )
    names(empty_quantile_df) <- quantile_nms
    empty_quantile_df <- as.data.frame(empty_quantile_df)
    if (length(group_vars) > 0){
      out <- df_row_slice(data2, group_starts)
      out <- f_bind_cols(out, df_init(empty_quantile_df, length(group_starts)))
    } else {
      out <- empty_quantile_df
    }
    return(reconstruct(data, out))
  }
  if (wide && (df_nrow(data) == 0L || length(probs) == 0L)) {
    if (length(group_vars) == 0){
      out <- matrix(integer(), ncol = length(quantile_out_nms),
                    nrow = 0)
      colnames(out) <- quantile_nms
      out <- reconstruct(data, as.data.frame(out))
    } else {
      out <- reconstruct(data, df_row_slice(f_select(data2, .cols = group_vars), group_starts))
    }
    return(out)
  }
  group_id_nm <- unique_col_name(data2, "group_id")
  data2 <- df_add_cols(data2, add_names(list(GRP_group_id(groups)), group_id_nm))
  q_df <- df_row_slice(data2, group_starts)
  group_lookup <- f_select(q_df, .cols = c(group_id_nm, group_vars))
  q_df <- df_rep(f_select(q_df, .cols = c(group_id_nm, dot_vars)), length(probs))
  q_df[[".quantile"]] <- rep(quantile_out_nms, each = df_nrow(q_df)/length(probs))
  if (length(dot_vars) > 0) {
    q_df <- dplyr::mutate(q_df, across(all_of(dot_vars), as.double))
  }
  seq_groups <- seq_len(n_groups)
  for (.col in dot_vars) {
    k <- 0L
    probi <- 0L
    for (p in probs) {
      p_seq <- seq_groups + probi
      probi <- probi + n_groups
      k <- k + 1L
      if (p == 0) {
        q_df[[.col]][p_seq] <-
          as.double(
            collapse::fmin(
              data2[[.col]], g = groups, na.rm = na.rm, use.g.names = FALSE
            )
          )
      }
      if (p == 1) {
        q_df[[.col]][p_seq] <-
          as.double(
            collapse::fmax(
              data2[[.col]], g = groups, na.rm = na.rm, use.g.names = FALSE
            )
          )
      }
      if (p > 0 && p < 1) {
        q_df[[.col]][p_seq] <-
          as.double(
            collapse::fnth(
              data2[[.col]], n = p, g = groups, na.rm = na.rm,
              use.g.names = FALSE, ties = quantile_ties
            )
          )
      }
    }
  }
  if (wide){
      q_df <- collapse::pivot(q_df, how = "wider", values = dot_vars,
                              names = ".quantile", sort = FALSE)
      out_nms <- c(group_vars, setdiff(names(q_df), c(group_vars, group_id_nm)))
  } else {
    out_nms <- c(
      group_vars,
      setdiff(names(q_df),
              c(group_vars, dot_vars, group_id_nm)),
      dot_vars
    )
  }

  # Join groups back onto final data frame
  out <- f_left_join(q_df, group_lookup, by = group_id_nm, multiple = FALSE)

  out <- f_select(out, .cols = out_nms)
  if (!.drop_groups){
    out <- reconstruct(data, out)
  }
  out
}
