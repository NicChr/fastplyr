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
#' To do the the latter type of by-group slicing, use `f_filter`, e.g. \cr
#' `f_filter(data, row_number() %in% slices, .by = groups)`
#' or even faster: \cr
#' `library(cheapr)` \cr
#' `f_filter(data, row_number() %in_% slices, .by = groups)`
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
#' @param ... A temporary argument to give the user an error if dots are used.
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
#' @param seed `r lifecycle::badge("deprecated")`
#' Use `cheapr::with_local_seed()` instead.
#' @param .order Should the groups be returned in sorted order?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#'
#' @returns
#' A `data.frame` filtered on the specified row indices.
#'
#' @rdname f_slice
#' @export
f_slice <- function(data, i = 0L, ..., .by = NULL,
                    .order = df_group_by_order_default(data),
                    keep_order = FALSE){
  rlang::check_dots_empty0(...)
  if (is.logical(i)){
    stop("i must be an integer vector, not a logical vector, use `f_filter()` instead")
  }
  if (length(i) == 0L){
    i <- 0L
  }
  i <- as.integer(i)
  N <- df_nrow(data)

  rng <- collapse::frange(i, na.rm = TRUE)
  rng_sum <- sum(sign(rng))
  if (abs(rng_sum) != 2){
    if (!any(rng == 0)){
      stop("Can't mix negative and positive locations")
    }
  }
  slice_sign <- sign(rng_sum)

  # Groups

  group_vars <- get_groups(data, .by = {{ .by }})

  if (length(group_vars) == 0L){
    if (any(abs(rng) > N)){
      data_locs <- i[which(dplyr::between(i, -N, N))]
    } else {
      data_locs <- cheapr::na_rm(i)
    }
  } else {
    groups <- data %>%
      group_collapse(.cols = group_vars, .add = TRUE,
                     order = .order)
    group_locs <- groups[[".loc"]]
    group_sizes <- groups[[".size"]]
    GN <- max(group_sizes)
    i <- i[which(dplyr::between(i, -GN, GN))]

    if (length(i) == 1 && slice_sign >= 1){
      data_locs <- cheapr::na_rm(list_subset(group_locs, i))
    } else {
      data_locs <- cpp_unlist_group_locs(cpp_slice_locs(group_locs, i))
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
#' @rdname f_slice
#' @export
f_slice_head <- function(data, n, prop, .by = NULL,
                         .order = df_group_by_order_default(data),
                         keep_order = FALSE){
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = .order,
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  # Start indices of sequences
  start <- sorted_group_starts(group_sizes)

  slice <- TRUE # Should data be sliced at the end?
  i <- integer()

  # Vectorised sequences
  if (length(slice_sizes) == 1){
    if (slice_sizes < df_nrow(data)){
      i <- seq_len(slice_sizes)
    } else {
      slice <- FALSE
    }
  } else {
    sequences <- sequence(slice_sizes, from = start, by = 1L)
    if (length(slice_sizes) > 1L){
      i <- cpp_unlist_group_locs(slice_info[["rows"]])[sequences]
    } else {
      i <- sequences
    }
  }
  if (keep_order){
    i <- sort(i)
  }
  if (slice){
    df_row_slice(data, i)
  } else {
    data
  }
}
#' @rdname f_slice
#' @export
f_slice_tail <- function(data, n, prop, .by = NULL,
                         .order = df_group_by_order_default(data),
                         keep_order = FALSE){
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = .order,
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  start <- sorted_group_ends(group_sizes)
  if (length(slice_sizes) == 1){
    i <- (start - slice_sizes + 1L):start
  } else {
    sequences <- sequence(slice_sizes, from = start - slice_sizes + 1L, by = 1L)
    if (length(slice_sizes) > 1L){
      i <- cpp_unlist_group_locs(slice_info[["rows"]])[sequences]
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
                       with_ties = TRUE, na_rm = FALSE,
                       .order = df_group_by_order_default(data),
                       keep_order = FALSE){
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- unique_col_name(names(data), "g")
  out <- data %>%
    add_group_id(.name = grp_nm1, .cols = group_vars, order = .order) %>%
    df_ungroup()

  g1 <- out[[grp_nm1]]
  out_info <- mutate_summary_grouped(out,
                                     !!rlang::enquo(order_by),
                                     .keep = "none",
                                     .by = all_of(grp_nm1))
  out <- out_info[["data"]]
  order_by_nm <- out_info[["cols"]]
  row_nm <- unique_col_name(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]], order = TRUE)
  # Order by Groups + desc order by var
  grp_nm <- unique_col_name(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(cheapr::new_df(g1 = g1, g2 = g2), order = TRUE)
  }
  out <- f_arrange(out, .cols = grp_nm)
  out1 <- f_slice_head(out, n = n, prop = prop, .by = all_of(grp_nm1),
                       .order = .order)
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
                       with_ties = TRUE, na_rm = FALSE,
                       .order = df_group_by_order_default(data),
                       keep_order = FALSE){
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- unique_col_name(names(data), "g")

  out <- data %>%
    add_group_id(.name = grp_nm1, .cols = group_vars, order = .order) %>%
    df_ungroup()

  g1 <- out[[grp_nm1]]
  out_info <- mutate_summary_grouped(out,
                                     !!rlang::enquo(order_by),
                                     .keep = "none",
                                     .by = all_of(grp_nm1))
  out <- out_info[["data"]]
  order_by_nm <- out_info[["cols"]]
  row_nm <- unique_col_name(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]], ascending = FALSE, order = TRUE)
  # Order by Groups + desc order by var
  grp_nm <- unique_col_name(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(cheapr::new_df(g1 = g1, g2 = g2), order = TRUE)
  }
  out <- f_arrange(out, .cols = grp_nm)
  out1 <- f_slice_head(out, n = n, prop = prop,
                       .by = all_of(grp_nm1), .order = .order)
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
                           .by = NULL, .order = df_group_by_order_default(data),
                           keep_order = FALSE,
                           weights = NULL, seed = NULL){
  # if (!is.null(seed)){
  #   lifecycle::deprecate_soft(
  #     "0.4.0", what = "f_slice_sample(seed)",
  #     details = "It is recommended to use `cheapr::with_local_seed(f_slice_sample())` instead."
  #   )
  # }
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
                                 sort_groups = .order,
                                 bound_n = (missing(n) && missing(prop)) || !replace,
                                 default_n = df_nrow(data))
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  n_groups <- length(group_sizes)
  rows <- cheapr::new_list(n_groups)
  if (has_weights){
    g <- cpp_df_group_indices(slice_info[["rows"]], df_nrow(data))
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
    on.exit({assign(".Random.seed", old, envir = globalenv())})
  } else if (!seed_is_null){
    on.exit({remove(".Random.seed", envir = globalenv())})
  }
  rows <- cpp_unlist_group_locs(rows)
  if (length(rows) > 0L){
    rows <- rows + rep.int(sorted_group_starts(group_sizes, 0L),
                           times = slice_sizes)
  }
  i <- cpp_unlist_group_locs(slice_info[["rows"]])[rows]
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
