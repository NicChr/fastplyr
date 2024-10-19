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
#'   tidy_quantiles(x)
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
  n_probs <- length(probs)
  quant_probs <- probs
  quant_ties <- paste0("q", type)
  q_prcnts <- quant_probs * 100
  quant_nms <- paste0(rep_len("p", length(quant_probs)), q_prcnts)
  quant_categories <- structure(
    strip_attrs(collapse::group(q_prcnts)),
    levels = collapse::funique(quant_nms), class = "factor"
  )

  data2 <- group_info[["data"]]
  groups <- df_to_GRP(data2, .cols = group_vars, order = .order)
  n_groups <- GRP_n_groups(groups)
  group_starts <- GRP_starts(groups)
  data2 <- f_select(data2, .cols = c(group_vars, dot_vars))

  # Some special cases

  if (wide && length(dot_vars) == 0){
    empty_quant_df <-
      cheapr::new_list(
        length(quant_nms),
        default = numeric()
      )
    names(empty_quant_df) <- quant_nms
    empty_quant_df <- as.data.frame(empty_quant_df)
    if (length(group_vars) > 0){
      out <- df_row_slice(data2, group_starts)
      out <- f_bind_cols(out, df_init(empty_quant_df, length(group_starts)))
    } else {
      out <- empty_quant_df
    }
    return(reconstruct(data, out))
  }
  if (wide && (df_nrow(data) == 0L || length(probs) == 0L)) {
    if (length(group_vars) == 0){
      out <- matrix(integer(), ncol = length(quant_categories),
                    nrow = 0)
      colnames(out) <- quant_nms
      out <- reconstruct(data, as.data.frame(out))
    } else {
      out <- reconstruct(data, df_row_slice(f_select(data2, .cols = group_vars), group_starts))
    }
    return(out)
  }

  ### Do this because if groups is a GRP then collapse::fnth allocates
  ### more memory than needed in the special case of <= 1 groups

  if (n_groups <= 1){
    collapse_groups <- NULL
  } else {
    collapse_groups <- groups
  }

  for (.col in dot_vars) {
    if (!is.integer(data2[[.col]])){
      data2[[.col]] <- as.numeric(data2[[.col]])
    }
  }

  if (wide){
    if (n_groups <= 1){
      out <- list(.quantile = quant_categories)
      for (.col in dot_vars){
        out[[.col]] <-
          as.double(
            collapse::fquantile(
              data2[[.col]], probs = probs, na.rm = na.rm,
              names = FALSE, type = type,
            )
          )
      }
      out <- list_as_df(out)
      out[[".temp.fastplyr.group.id"]] <- 0L
      out <- collapse::pivot(
        out, how = "wider", values = dot_vars,
        names = ".quantile", sort = FALSE
      )
      out[[".temp.fastplyr.group.id"]] <- NULL
    } else {
      out <- df_row_slice(f_select(data2, .cols = group_vars), group_starts)
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
        for (p in probs) {
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
                  use.g.names = FALSE, ties = quant_ties
                )
              )
          }
          out[[k]] <- sample_quantiles
          k <- k + 1L
        }
      }
      out <- list_as_df(out)

    }

  } else {
    if (n_groups <= 1){

      out <- list(.quantile = quant_categories)
      for (.col in dot_vars){
        out[[.col]] <-
          as.double(
            collapse::fquantile(
              data2[[.col]], probs = probs, na.rm = na.rm,
              names = FALSE, type = type,
            )
          )
      }
      out <- list_as_df(out)
    } else {

      # Start of grouped method

      out <- df_row_slice(data2, group_starts)
      out <- df_rep_each(out, n_probs)
      out[[".quantile"]] <- rep(quant_categories, df_nrow(out) / n_probs)
      out <- f_select(out, .cols = c(group_vars, ".quantile", dot_vars))
      if (length(dot_vars) > 0) {
        out <- dplyr::mutate(
          out, dplyr::across(dplyr::all_of(dot_vars), as.double)
        )
      }
      quant_starts <- ( length(probs) * (seq_len(n_groups) - 1L) ) + 1L

      for (.col in dot_vars) {
        k <- 0L
        for (p in probs) {
          p_seq <- quant_starts + k
          k <- k + 1L
          if (p == 0) {
            cpp_set_replace(
              out[[.col]], p_seq,
              as.double(
                collapse::fmin(
                  data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
                )
              )
            )
          } else if (p == 1) {
            cpp_set_replace(
              out[[.col]], p_seq,
              as.double(
                collapse::fmax(
                  data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
                )
              )
            )
          } else if (p > 0 && p < 1) {
            cpp_set_replace(
              out[[.col]], p_seq,
              as.double(
                collapse::fnth(
                  data2[[.col]], n = p, g = collapse_groups, na.rm = na.rm,
                  use.g.names = FALSE, ties = quant_ties
                )
              )
            )
          }
        }
      }

      # End of grouped method

    }
  }

  if (.drop_groups){
    reconstruct(df_ungroup(data), out)
  } else {
    reconstruct(data, out)
  }
}
# tidy_quantiles <- function(data, ..., probs = seq(0, 1, 0.25),
#                            type = 7, pivot = c("wide", "long"),
#                            na.rm = TRUE,
#                            .by = NULL, .cols = NULL,
#                            .order = df_group_by_order_default(data),
#                            .drop_groups = TRUE){
#   pivot <- rlang::arg_match(pivot)
#   wide <- pivot == "wide"
#   group_info <- tidy_group_info(
#     data, ..., .by = {{ .by }}, .cols = .cols, ungroup = TRUE
#   )
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#
#   # Constructing quantile info
#   n_probs <- length(probs)
#   quantile_probs <- probs
#   quantile_ties <- paste0("q", type)
#   q_prcnts <- quantile_probs * 100
#   quantile_nms <- paste0(rep_len("p", length(quantile_probs)),
#                          q_prcnts)
#   quantile_out_nms <- structure(strip_attrs(collapse::group(q_prcnts)),
#                                 levels = collapse::funique(quantile_nms), class = "factor")
#
#   data2 <- group_info[["data"]]
#   groups <- df_to_GRP(data2, .cols = group_vars, order = .order)
#   n_groups <- GRP_n_groups(groups)
#   group_starts <- GRP_starts(groups)
#   data2 <- f_select(data2, .cols = c(group_vars, dot_vars))
#
#   # Some special cases
#
#   if (wide && length(dot_vars) == 0){
#     empty_quantile_df <-
#       cheapr::new_list(
#         length(quantile_nms),
#         default = numeric()
#       )
#     names(empty_quantile_df) <- quantile_nms
#     empty_quantile_df <- as.data.frame(empty_quantile_df)
#     if (length(group_vars) > 0){
#       out <- df_row_slice(data2, group_starts)
#       out <- f_bind_cols(out, df_init(empty_quantile_df, length(group_starts)))
#     } else {
#       out <- empty_quantile_df
#     }
#     return(reconstruct(data, out))
#   }
#   if (wide && (df_nrow(data) == 0L || length(probs) == 0L)) {
#     if (length(group_vars) == 0){
#       out <- matrix(integer(), ncol = length(quantile_out_nms),
#                     nrow = 0)
#       colnames(out) <- quantile_nms
#       out <- reconstruct(data, as.data.frame(out))
#     } else {
#       out <- reconstruct(data, df_row_slice(f_select(data2, .cols = group_vars), group_starts))
#     }
#     return(out)
#   }
#
#   ### Do this because if groups is a GRP then collapse::fnth allocates
#   ### more memory than needed in the special case of <= 1 groups
#
#   if (n_groups <= 1){
#     collapse_groups <- NULL
#   } else {
#     collapse_groups <- groups
#   }
#
#   for (.col in dot_vars) {
#     if (!is.integer(data2[[.col]])){
#       data2[[.col]] <- as.numeric(data2[[.col]])
#     }
#   }
#
#   if (wide){
#     out <- df_row_slice(f_select(data2, .cols = group_vars), group_starts)
#     # Allocate enough space
#     out <- c(as.list(out), vector("list", length(dot_vars) * n_probs))
#     if (length(dot_vars) == 1){
#       names(out) <- c(group_vars, quantile_nms)
#     } else {
#       names(out) <- c(group_vars, paste(rep(dot_vars, each = n_probs),
#                                         quantile_nms, sep = "_"))
#     }
#     k <- 1L
#     for (.col in dot_vars) {
#       # k <- 1L
#       for (p in probs) {
#         if (p == 0) {
#           sample_quantiles <-
#             as.double(
#               collapse::fmin(
#                 data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
#               )
#             )
#         }
#         if (p == 1) {
#           sample_quantiles <-
#             as.double(
#               collapse::fmax(
#                 data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
#               )
#             )
#         }
#         if (p > 0 && p < 1) {
#           sample_quantiles <-
#             as.double(
#               collapse::fnth(
#                 data2[[.col]], n = p, g = collapse_groups, na.rm = na.rm,
#                 use.g.names = FALSE, ties = quantile_ties
#               )
#             )
#         }
#         # if (length(dot_vars) == 1){
#         #   col_nm <- quantile_nms[k]
#         # } else {
#         #   col_nm <- paste(.col, quantile_nms[k], sep = "_")
#         # }
#         # out[[col_nm]] <- sample_quantiles
#         out[[k]] <- sample_quantiles
#         k <- k + 1L
#       }
#     }
#     out <- list_as_tbl(out)
#   } else {
#     out <- df_row_slice(data2, group_starts)
#     out <- df_rep_each(out, n_probs)
#     out[[".quantile"]] <- rep(quantile_out_nms, df_nrow(out) / n_probs)
#     out <- f_select(out, .cols = c(group_vars, ".quantile", dot_vars))
#     if (length(dot_vars) > 0) {
#       out <- dplyr::mutate(
#         out, dplyr::across(dplyr::all_of(dot_vars), as.double)
#       )
#     }
#     quantile_starts <- ( length(probs) * (seq_len(n_groups) - 1L) ) + 1L
#
#     for (.col in dot_vars) {
#       k <- 0L
#       for (p in probs) {
#         p_seq <- quantile_starts + k
#         k <- k + 1L
#         if (p == 0) {
#           cpp_set_replace(
#             out[[.col]], p_seq,
#             as.double(
#               collapse::fmin(
#                 data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
#               )
#             )
#           )
#         }
#         if (p == 1) {
#           cpp_set_replace(
#             out[[.col]], p_seq,
#             as.double(
#               collapse::fmax(
#                 data2[[.col]], g = collapse_groups, na.rm = na.rm, use.g.names = FALSE
#               )
#             )
#           )
#         }
#         if (p > 0 && p < 1) {
#           cpp_set_replace(
#             out[[.col]], p_seq,
#             as.double(
#               collapse::fnth(
#                 data2[[.col]], n = p, g = collapse_groups, na.rm = na.rm,
#                 use.g.names = FALSE, ties = quantile_ties
#               )
#             )
#           )
#         }
#       }
#     }
#   }
#
#   if (.drop_groups){
#     reconstruct(df_ungroup(data), out)
#   } else {
#     reconstruct(data, out)
#   }
# }


# Using collapse::fsummarise()

fast_tidy_quantiles <- function(data, ..., probs = seq(0, 1, 0.25),
                            pivot = c("wide", "long"),
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
  n_probs <- length(probs)
  quantile_probs <- probs
  q_prcnts <- quantile_probs * 100
  quantile_nms <- paste0(rep_len("p", length(quantile_probs)), q_prcnts)
  quantile_out_nms <- structure(
    strip_attrs(collapse::group(q_prcnts)),
    levels = collapse::funique(quantile_nms), class = "factor"
  )

  data2 <- group_info[["data"]] %>%
    add_group_id(.cols = group_vars, .order = .order, as_qg = TRUE)
  group_id_nm <- names(data2)[df_ncol(data2)]
  n_groups <- attr(data2[[group_id_nm]], "N.groups")
  group_starts <- attr(data2[[group_id_nm]], "starts")
  group_sizes <- attr(data2[[group_id_nm]], "group.sizes")

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

  for (.col in dot_vars) {
    if (!is.integer(data2[[.col]])){
      data2[[.col]] <- as.numeric(data2[[.col]])
    }
  }

  if (n_groups > 1){
    groups <- new_GRP(
      n_groups, strip_attrs(data2[[group_id_nm]]),
      attr(data2[[group_id_nm]], "group.sizes"),
      group.starts = group_starts,
      ordered = c("ordered" = .order, "sorted" = NA)
    )
    attr(data2, "groups") <- groups
    class(data2) <- c("GRP_df", "tbl_df", "tbl", "grouped_df", "data.frame")
  } else {
    groups <- NULL
  }

  if (wide){
    out <- new_tbl(.nrows = 1L)
    bind <- function(...){
      f_bind_cols(..., .repair_names = FALSE)
    }
  } else {
    out <- f_slice(
      f_select(as_tbl(data2), .cols = dot_vars), 0L
    )
    bind <- f_bind_rows
  }
  for (i in seq_along(quantile_probs)){
    prob <- quantile_probs[i]
    prob_nm <- quantile_nms[i]
    if (prob == 0){
      out <- bind(
        out,
        f_select(
          collapse::fsummarise(
            data2,
            across(dot_vars, collapse::fmin)
          ), .cols = dot_vars
        )
      )
    } else if (prob == 1){
      out <- bind(
        out,
        f_select(
          collapse::fsummarise(
            data2,
            across(dot_vars, collapse::fmax)
          ), .cols = dot_vars
        )
      )
    } else {
      out <- bind(
        out,
        f_select(
          collapse::fsummarise(
            data2,
            across(dot_vars, collapse::fnth, prob)
          ), .cols = dot_vars
        )
      )
    }
  }
  join_groups <- df_row_slice(
    f_select(as_tbl(data2), .cols = group_vars),
    group_starts
  )
  if (pivot == "wide"){
    out <- f_select(out, .cols = order(match(names(out), dot_vars)))
    if (length(dot_vars) > 1){
      names(out) <- paste(names(out), quantile_nms, sep = "_")
    } else {
      names(out) <- quantile_nms
    }
    out <- f_bind_cols(join_groups, out)
  } else {
    join_groups <- df_rep(
      join_groups, length(quantile_nms)
    )
    out <- f_bind_cols(
      join_groups,
      .quantile = rep(quantile_out_nms, each = df_nrow(out) / length(quantile_out_nms)),
      out
    )
    if (n_groups > 1){
      out <- df_row_slice(
        out,
        order(rep(groups[["group.id"]][group_starts], length(quantile_nms)))
      )
    }
  }
    if (.drop_groups){
    reconstruct(df_ungroup(data), out)
  } else {
    reconstruct(data, out)
  }
}
