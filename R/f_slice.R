#' Faster `dplyr::slice()`
#'
#' @description
#' When there are lots of groups, the `f_slice()` functions are much faster.
#'
#' @details
#'
#' ### Important note about the `i` argument in `f_slice`
#'
#' `i` is first evaluated on an un-grouped basis and then searches for
#' those locations in each group. Thus if you supply an expression
#' of slice locations that vary by-group, this will not be respected nor checked.
#' For example, \cr
#' do `f_slice(data, 10:20, .by = group)` \cr
#' not `f_slice(data, sample(1:10), .by = group)`. \cr
#'
#' The former results in slice locations that do not vary by group but the latter
#' will result in different within-group slice locations which `f_slice` cannot
#' correctly compute.
#'
#' To do the the latter type of by-group slicing, use `f_filter`, e.g.
#' `f_filter(data, row_number() %in% slices, .by = groups)`
#'
#' ### `f_slice_sample`
#'
#' The arguments of `f_slice_sample()` align more closely with `base::sample()` and thus
#' by default re-samples each entire group without replacement.
#'
#' @param data A data frame.
#' @param i An [integer] vector of slice locations. \cr
#' Please see the details below on how `i` works as it
#' only accepts simple integer vectors.
#' @param keep_order Should the sliced data frame be returned in its original order?
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param n Number of rows.
#' @param prop Proportion of rows.
#' @param order_by Variables to order by.
#' @param with_ties Should ties be kept together? The default is `TRUE`.
#' @param na_rm Should missing values in `f_slice_max()` and `f_slice_min()` be removed?
#' The default is `FALSE`.
#' @param replace Should `f_slice_sample()` sample with or without replacement?
#' Default is `FALSE`, without replacement.
#' @param weights Probability weights used in `f_slice_sample()`.
#' @param seed Seed number defining RNG state.
#' If supplied, this is only applied \bold{locally} within the function
#' and the seed state isn't retained after sampling.
#' To clarify, whatever seed state was in place before the function call,
#' is restored to ensure seed continuity.
#' If left `NULL` (the default), then the seed is never modified.
#'
#' @returns
#' A `data.frame` of specified rows.
#'
#' @rdname f_slice
#' @export
f_slice <- function(data, i, .by = NULL, keep_order = FALSE){
  if (is.logical(i)){
    stop("i must be an integer vector, not a logical vector, use `f_filter()` instead")
  }
  if (length(i) == 0L){
    i <- 0L
  }
  N <- df_nrow(data)

  rng <- collapse::frange(i, na.rm = FALSE)
  rng_sum <- sum(sign(1 / rng))
  if (abs(rng_sum) != 2){
    stop("Can't mix negative and positive locations")
  }
  slice_sign <- sign(rng_sum)

  # Groups

  group_vars <- get_groups(data, .by = {{ .by }})

  if (length(group_vars) == 0L){
    if (any(abs(rng) > N)){
      data_locs <- i[which(dplyr::between(i, -N, N))]
    } else {
      data_locs <- i
    }
  } else {
    groups <- data %>%
      group_collapse(.cols = group_vars, .add = TRUE)
    group_locs <- groups[[".loc"]]
    group_sizes <- groups[[".size"]]
    # groups <- data %>%
    #   f_group_by(.cols = group_vars, order = df_group_by_order_default(data), .add = TRUE) %>%
    #   group_data()
    # group_locs <- groups[[".rows"]]
    # group_sizes <- cheapr::lengths_(group_locs)
    # Constrain n to <= max GRPN
    GN <-  max(group_sizes)
    i <- i[which(dplyr::between(i, -GN, GN))]
    if (slice_sign >= 1){
      size <- pmin.int(max(i), group_sizes)
    } else {
      size <- pmax.int(0L, group_sizes - max(abs(i)))
    }
    keep <- cheapr::which_val(size, 0, invert = TRUE)
    if (length(group_locs) - length(keep) > 0L){
      group_locs <- group_locs[keep]
      row_lens <- row_lens[keep]
      size <- size[keep]
    }
    if (length(i) == 1 && slice_sign >= 1){
      data_locs <- list_subset(group_locs, i)
      data_locs <- cheapr::na_rm(data_locs)
    } else {
      data_locs <- unlist(cpp_slice_locs(group_locs, i))
    }
    if (is.null(data_locs)){
      data_locs <- integer(0)
    }
  }
  if (keep_order){
    data_locs <- sort(data_locs)
  }
  df_row_slice(data, data_locs)
}
# f_slice <- function(data, i, .by = NULL,
#                     keep_order = FALSE,
#                     sort_groups = df_group_by_order_default(data)){
#   group_vars <- get_groups(data, .by = {{ .by }})
#   temp <- data %>%
#     f_group_by(.cols = group_vars, .add = TRUE, order = sort_groups) %>%
#     dplyr::reframe({{ i }}) %>%
#     f_group_by(.cols = group_vars, order = sort_groups)
#   slice_var <- setdiff(names(temp), group_vars)
#   if (is.logical(temp[[slice_var]])){
#     stop("must be an integer vector, not a logical vector, use `f_filter()` instead")
#   }
#
#   temp_groups <- group_data(temp)
#   slice_counts <- df_count(temp)
#
#   ## Build the group IDS as a qG as collapse stat functions can run faster this way
#   group_ids <- df_group_id(temp)
#   attr(group_ids, "N.groups") <- nrow(temp_groups)
#   attr(group_ids, "starts") <- list_subset(temp_groups[[".rows"]], 1L)
#   attr(group_ids, "group.sizes") <- slice_counts[[ncol(slice_counts)]]
#   class(group_ids) <- c("qG", "na.included")
#   temp[[slice_var]] <- as.integer(temp[[slice_var]])
#   slice_locs <- temp[[slice_var]]
#
#   slice_counts[["slice_means"]] <- collapse::fmean(slice_locs, g = group_ids, use.g.names = FALSE)
#   slice_counts[["slice_mins"]] <- collapse::fmin(slice_locs, g = group_ids, use.g.names = FALSE)
#   slice_counts[["slice_maxs"]] <- collapse::fmax(slice_locs, g = group_ids, use.g.names = FALSE)
#
#   ## The idea is if there is no variation in calculated by-group locs
#   ## then we can use a much more efficient method to slice
#
#   use_simpler_slicing <- collapse::fnunique(f_select(slice_counts, .cols = 2:5)) == 1
#
#   if (!use_simpler_slicing){
#     groups <- data %>%
#       f_group_by(.cols = group_vars, order = sort_groups, .add = TRUE) %>%
#       group_data()
#
#     group_locs <- groups[[".rows"]]
#     # data_locs <- vector("list", nrow(groups))
#     by_group_slice_locs <- gsplit2(slice_locs, group_ids)
#
#     # for (i in df_seq_along(groups)){
#     #   data_locs[[i]] <- cpp_int_slice(group_locs[[i]], by_group_slice_locs[[i]], check = TRUE)
#     # }
#     data_locs <- unlist(cpp_slice_locs(group_locs, by_group_slice_locs))
#   } else {
#     if (is.logical(i)){
#       stop("i must be an integer vector, not a logical vector, use `f_filter()` instead")
#     }
#     N <- df_nrow(data)
#     if (length(i) == 0L){
#       i <- 0L
#     }
#     rng <- collapse::frange(i, na.rm = FALSE)
#     rng_sum <- sum(sign(1 / rng))
#     if (abs(rng_sum) != 2){
#       stop("Can't mix negative and positive locations")
#     }
#     slice_sign <- sign(rng_sum)
#     # Groups
#     # group_vars <- get_groups(data, .by = {{ .by }})
#     if (length(group_vars) == 0L){
#       if (any(abs(rng) > N)){
#         slice_locs <- i[which(dplyr::between(i, -N, N))]
#       } else {
#         slice_locs <- i
#       }
#     } else {
#       group_df <- group_collapse(data, .by = {{ .by }},
#                                  order = sort_groups, sort = sort_groups,
#                                  id = FALSE, loc = TRUE,
#                                  size = TRUE, start = FALSE, end = FALSE)
#       # Constrain n to <= max GRPN
#       GN <-  max(group_df[[".size"]])
#       i <- i[which(dplyr::between(i, -GN, GN))]
#       rows <- group_df[[".loc"]]
#       row_lens <- group_df[[".size"]]
#       if (slice_sign >= 1){
#         size <- pmin.int(max(i), row_lens)
#       } else {
#         size <- pmax.int(0L, row_lens - max(abs(i)))
#       }
#       keep <- cheapr::which_val(size, 0, invert = TRUE)
#       if (length(rows) - length(keep) > 0L){
#         rows <- rows[keep]
#         row_lens <- row_lens[keep]
#         size <- size[keep]
#       }
#       if (length(i) == 1 && slice_sign >= 1){
#         data_locs <- list_subset(rows, i)
#         data_locs <- data_locs[cheapr::which_not_na(data_locs)]
#       } else {
#         data_locs <- cpp_slice_locs(rows, i)
#         # data_locs <- vector("list", length(rows))
#         # for (j in seq_along(data_locs)){
#         #   data_locs[[j]] <- cpp_int_slice(.subset2(rows, j), i, TRUE)
#         # }
#         # data_locs <- unlist(data_locs, use.names = FALSE, recursive = FALSE)
#       }
#       if (is.null(data_locs)){
#         data_locs <- integer(0)
#       }
#     }
#     if (keep_order){
#       data_locs <- sort(data_locs)
#     }
#   }
#   df_row_slice(data, data_locs)
# }
#' @rdname f_slice
#' @export
f_slice_head <- function(data, n, prop, .by = NULL, keep_order = FALSE){
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = df_group_by_order_default(data),
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  # Start indices of sequences
  start <- calc_sorted_group_starts(group_sizes)
  # Vectorised sequences
  if (length(slice_sizes) == 1){
    i <- seq_len(slice_sizes)
  } else {
    sequences <- sequence(slice_sizes, from = start, by = 1L)
    if (length(slice_sizes) > 1L){
      i <- unlist(slice_info[["rows"]], recursive = FALSE, use.names = FALSE)[sequences]
    } else {
      i <- sequences
    }
  }
  if (keep_order){
    i <- sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname f_slice
#' @export
f_slice_tail <- function(data, n, prop, .by = NULL, keep_order = FALSE){
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = df_group_by_order_default(data),
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  start <- calc_sorted_group_ends(group_sizes)
  sequences <- sequence(slice_sizes, from = start - slice_sizes + 1L, by = 1L)
  if (length(slice_sizes) == 1){
    i <- (start -slice_sizes + 1L):start
  } else {
    if (length(slice_sizes) > 1L){
      i <- unlist(slice_info[["rows"]], recursive = FALSE, use.names = FALSE)[sequences]
    } else {
      i <- sequences
    }
  }
  if (keep_order){
    i <- sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname f_slice
#' @export
f_slice_min <- function(data, order_by, n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE, keep_order = FALSE){
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- unique_col_name(names(data), "g")
  out <- df_ungroup(data)
  g1 <- group_id(f_select(data, .cols = group_vars), order = df_group_by_order_default(data))
  out[[grp_nm1]] <- g1
  out_info <- mutate_summary_grouped(out,
                                     !!rlang::enquo(order_by),
                                     .keep = "none",
                                     .by = all_of(grp_nm1))
  out <- out_info[["data"]]
  order_by_nm <- out_info[["cols"]]
  row_nm <- unique_col_name(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]])
  # Order by Groups + desc order by var
  grp_nm <- unique_col_name(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(new_df(g1 = g1, g2 = g2))
  }
  out <- f_arrange(out, .cols = grp_nm)
  out1 <- f_slice_head(out, n = n, prop = prop, .by = all_of(grp_nm1))
  if (with_ties){
    i <- out[[row_nm]][cheapr::which_not_na(
      collapse::fmatch(out[[grp_nm]], out1[[grp_nm]], overid = 2L)
    )]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][cheapr::which_na(out[[order_by_nm]])]
    i <- cheapr::setdiff_(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname f_slice
#' @export
f_slice_max <- function(data, order_by, n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE, keep_order = FALSE){
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- unique_col_name(names(data), "g")
  out <- df_ungroup(data)
  g1 <- group_id(f_select(data, .cols = group_vars), order = df_group_by_order_default(data))
  out[[grp_nm1]] <- g1
  out_info <- mutate_summary_grouped(out,
                                     !!rlang::enquo(order_by),
                                     .keep = "none",
                                     .by = all_of(grp_nm1))
  out <- out_info[["data"]]
  order_by_nm <- out_info[["cols"]]
  row_nm <- unique_col_name(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]], ascending = FALSE)
  # Order by Groups + desc order by var
  grp_nm <- unique_col_name(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(new_df(g1 = g1, g2 = g2))
  }
  out <- f_arrange(out, .cols = grp_nm)
  out1 <- f_slice_head(out, n = n, prop = prop, .by = all_of(grp_nm1))
  if (with_ties){
    i <- out[[row_nm]][cheapr::which_not_na(
      collapse::fmatch(out[[grp_nm]], out1[[grp_nm]], overid = 2L)
    )]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][cheapr::which_na(out[[order_by_nm]])]
    i <- cheapr::setdiff_(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname f_slice
#' @export
f_slice_sample <- function(data, n, replace = FALSE, prop,
                          .by = NULL,
                          keep_order = FALSE,
                          weights = NULL, seed = NULL){
  # Check if a seed already exists in global environment
  seed_exists <- exists(".Random.seed")
  # Save it in the first instance
  if (seed_exists){
    old <- .Random.seed
  }
  groups <- get_groups(data, .by = {{ .by }})
  # Does user want to use local seed?
  seed_is_null <- is.null(seed)
  has_weights <- !rlang::quo_is_null(rlang::enquo(weights))
  if (has_weights){
    data_info  <- mutate_summary_grouped(data, !!rlang::enquo(weights))
    data <- data_info[["data"]]
    weights_var <- data_info[["cols"]]
  }
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = df_group_by_order_default(data),
                                 bound_n = (missing(n) && missing(prop)) || !replace,
                                 default_n = df_nrow(data))
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  rows <- vector("list", length(slice_info[["rows"]]))
  if (has_weights){
    g <- group_id(f_select(data, .cols = groups), order = df_group_by_order_default(data))
    weights <- gsplit2(data[[weights_var]], g = g)
  } else {
    weights <- NULL
  }
  # If user wants to use local seed
  # We must first save the current seed
  # Set the new seed
  # Discard the newly created seed after sampling
  # Restore the old seed (if there existed an old seed)
  if (!seed_is_null){
    set.seed(seed)
  }
  for (i in seq_along(rows)){
    rows[[i]] <- sample.int(.subset2(group_sizes, i),
                            size = .subset2(slice_sizes, i),
                            replace = replace,
                            prob = .subset2(weights, i))
  }
  if (seed_exists && !seed_is_null){
    on.exit({ assign(".Random.seed", old, envir = globalenv())})
  } else if (!seed_is_null){
    on.exit({remove(".Random.seed", envir = globalenv())})
  }
  rows <- unlist(rows, use.names = FALSE, recursive = FALSE)
  if (length(rows) > 0L){
    rows <- rows + rep.int(calc_sorted_group_starts(group_sizes, 0L),
                           times = slice_sizes)
  }
  i <- unlist(slice_info[["rows"]], use.names = FALSE, recursive = FALSE)[rows]
  if (is.null(i)){
    i <- integer()
  }
  if (keep_order){
    i <- sort(i)
  }
  df_row_slice(data, i)
}
df_slice_prepare <- function(data, n, prop, .by = NULL,
                             sort_groups = df_group_by_order_default(data),
                             bound_n = TRUE, default_n = 1L){
  N <- df_nrow(data)
  missing_n <- missing(n)
  missing_prop <- missing(prop)
  if (!missing_n && !missing_prop){
    stop("Either n or prop must be supplied, not both.")
  }
  if (missing_n && missing_prop){
    n <- default_n
    type <- "n"
  }
  if (!missing_n && missing_prop){
    check_length(n, 1L)
    type <- "n"
  }
  if (missing_n && !missing_prop){
    check_length(prop, 1L)
    type <- "prop"
  }

  group_df <- group_collapse(data, .by = {{ .by }},
                             order = sort_groups, sort = sort_groups,
                             id = FALSE, loc = TRUE,
                             # loc_order = FALSE,
                             size = TRUE, start = FALSE, end = FALSE)
  rows <- group_df[[".loc"]]
  group_sizes <- group_df[[".size"]]
  if (type == "n"){
    # USING N
    if (bound_n){
      GN <- collapse::fmax(group_sizes, use.g.names = FALSE, na.rm = FALSE)
      if (sign(1/n) >= 1){
        n <- as.integer(min(n, GN))
        slice_sizes <- pmin.int(n, group_sizes)
      } else {
        n <- as.integer(max(n, -GN))
        slice_sizes <- pmax.int(0L, group_sizes + n)
      }
    } else {
      slice_sizes <- rep_len(as.integer(n), length(rows))
    }
  } else {
    # USING prop
    if (bound_n){
      if (sign(1/prop) >= 1){
        prop <- min(1, prop)
        slice_sizes <- floor(prop * group_sizes)
      } else {
        prop <- max(-1, prop)
        slice_sizes <- ceiling( (1 + prop) * group_sizes)
      }
    } else {
      slice_sizes <- prop * group_sizes
    }
    slice_sizes <- as.integer(slice_sizes)
  }
  keep <- which(slice_sizes > 0)
  if (length(rows) - length(keep) > 0L){
    rows <- rows[keep]
    group_sizes <- group_sizes[keep]
    slice_sizes <- slice_sizes[keep]
  }
  list("rows" = rows,
       "group_sizes" = group_sizes,
       "slice_sizes" = slice_sizes)
}
