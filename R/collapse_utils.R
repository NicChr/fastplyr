
## GRP() with separate methods for data frames
GRP2 <- function(X, by = NULL, sort = TRUE,
                 return.order = sort,
                 return.groups = TRUE,
                 return.locs = FALSE,
                 ...){
  if (is_GRP(X)){
    return(X)
  } else if (is_df(X)){
    df_to_GRP(X, .cols = by %||% names(X), order = sort,
              return.order = return.order,
              return.groups = return.groups,
              return.locs = return.locs)
  } else {
    GRP3(
      X, by = by, sort = sort,
      return.order = return.order,
      return.groups = return.groups,
      return.locs = return.locs,
      ...
    )
  }
}

## GRP() but always returns group starts
GRP3 <- function(X, by = NULL, sort = TRUE,
                 return.order = sort,
                 return.groups = TRUE,
                 return.locs = FALSE,
                 call = FALSE, ...){
  if (is_GRP(X)){
    return(X)
  }
  if (!sort && !return.groups && !is.factor(X)) {
    if (!is.null(by)) {
      X <- cheapr::sset_df(X, j = by)
    }
    out <- cheapr::new_list(10L)
    # groups <- group2(X)
    groups <- group3(X, starts = TRUE, group.sizes = TRUE)
    out[[1L]] <- attr(groups, "N.groups")
    out[[3L]] <- attr(groups, "group.sizes")
    out[[8L]] <- attr(groups, "starts")
    if (is.factor(X)) {
      out[[6L]] <- add_names(c(NA, TRUE), c("ordered","sorted"))
    }
    else {
      out[[6L]] <- add_names(c(FALSE, NA), c("ordered", "sorted"))
    }
    strip_attrs(groups, set = TRUE)
    out[[2L]] <- groups
    names(out) <- c("N.groups", "group.id",
                    "group.sizes", "groups", "group.vars", "ordered",
                    "order", "group.starts", "call", "locs")
    class(out) <- "GRP"
  }
  else {
    out <- collapse::GRP(
      X, by = by, sort = sort, return.order = return.order,
      return.groups = return.groups, call = call, ...
    )
  }
  if (!is.null(out)){
    out[[8L]] <- GRP_starts(out)
    out <- c(out, list(locs = NULL))
    if (return.locs){
      out[[10L]] <- GRP_loc(out)
    }
    class(out) <- "GRP"
  }
  out
}
# Two alternatives to collapse::group
# They both handle nested data frames

# group2() is simpler (and better) but has 'large' overhead
group2 <- function(X){
  group_id(X, order = FALSE, as_qg = TRUE)
}
# Lower overhead than group2() and is essentially an extension
# to collapse::group() for list objects, etc
group3 <- function(X, starts = FALSE, group.sizes = FALSE){
  if (is.null(X)) {
    return(NULL)
  }
  if (inherits(X, "data.frame")) {
    if (df_ncol(X) == 0){
      N <- df_nrow(X)
      out <- rep_len(1L, N)
      attr(out, "N.groups") <- min(1L, N)
      if (starts) {
        attr(out, "starts") <- if (N == 0)
          integer()
        else 1L
      }
      if (group.sizes) {
        attr(out, "group.sizes") <- if (N == 0)
          integer()
        else N
      }
      attr(out, "class") <- c("qG", "na.included")
      return(out)
    } else {
      X <- df_mutate_exotic_to_ids(X, order = FALSE)
    }
  } else if (cpp_is_exotic(X)){

    # Essentially group2() as above
    out <- group_id(X, order = FALSE, as_qg = TRUE)
    if (!starts){
      attr(out, "starts") <- NULL
    }
    if (!group.sizes){
      attr(out, "group.sizes") <- NULL
    }
    return(out)
  }
  collapse::group(X, starts = starts, group.sizes = group.sizes)
}

# Is object a collapse GRP?
is_GRP <- function(GRP){
  inherits(GRP, "GRP")
}
check_GRP <- function(GRP){
  if (!is_GRP(GRP)){
    cli::cli_abort("{.arg GRP} must be a {.cls GRP}")
  }
}
# Number of groups
GRP_n_groups <- function(GRP){
  GRP[["N.groups"]]
}
# Group IDs (integer)
GRP_group_id <- function(GRP){
  GRP[["group.id"]]
}
GRP_data_size <- function(GRP){
  length(GRP_group_id(GRP))
}
# Group sizes
GRP_group_sizes <- function(GRP){
  GRP[["group.sizes"]]
}
GRP_expanded_group_sizes <- function(GRP){
  GRP_group_sizes(GRP)[GRP_group_id(GRP)]
}
# Groups
GRP_groups <- function(GRP){
  GRP[["groups"]]
}
# Group variable names
GRP_group_vars <- function(GRP){
  GRP[["group.vars"]]
}
check_GRP_has_groups <- function(GRP){
  if (is_GRP(GRP) && is.null(GRP_groups(GRP))){
    stop("GRP has no group data. Please supply a GRP object with group data")
  }
}
check_data_GRP_size <- function(x, GRP){
  if (!is.null(GRP)){
    size <- NROW(x)
    GRP_size <- GRP_data_size(GRP)
    if (size != GRP_size){
      stop("size of x must match size of groups")
    }
  }
}

# Alternate mixed method
# GRP_duplicated <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   if (is.null(GRP[["order"]])){
#     out <- (sizes > 1L)[group_id]
#     if (!all){
#       out[GRP_starts(GRP)] <- FALSE
#     }
#   } else {
#     if (all){
#       out <- (sizes > 1L)[group_id]
#     } else {
#       out <- row_id(GRP) > 1L
#     }
#   }
#   out
# }

GRP_duplicated <- function(GRP, all = FALSE){
  sizes <- GRP_group_sizes(GRP)
  group_id <- GRP_group_id(GRP)
  out <- (sizes > 1L)[group_id]
  if (!all){
    out[GRP_starts(GRP)] <- FALSE
  }
  out
}
# Alternate version
# GRP_which_duplicated <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   if (all){
#     which((sizes > 1L)[group_id])
#   } else {
#     which_val(row_id(GRP), 1L, invert = TRUE)
#   }
# }
GRP_which_duplicated <- function(GRP, all = FALSE){
  which(GRP_duplicated(GRP, all))
}
sorted_group_starts <- function(group_sizes, init_loc = 1L){
  cpp_sorted_group_starts(as.integer(group_sizes), init_loc)
}
sorted_group_ends <- function(group_sizes){
  collapse::fcumsum(group_sizes)
}
GRP_starts <- function(GRP, use.g.names = FALSE){
  out <- GRP[["group.starts"]]
  if (is.null(out)){
    GRP_sizes <- GRP_group_sizes(GRP)
    if (GRP_is_sorted(GRP)){
      sorted_starts <- attr(GRP[["order"]], "starts")
      if (!is.null(sorted_starts)){
        out <- sorted_starts
      } else {
        out <- sorted_group_starts(GRP_sizes)
      }
      # For factors with 0 size, replace calculated group starts with 0
      out[cheapr::which_val(GRP_sizes, 0L)] <- 0L
    } else {
      o <- GRP_order(GRP)
      starts <- attr(o, "starts")
      if (collapse::anyv(GRP_sizes, 0L)){
        out <- integer(GRP_n_groups(GRP))
        out[cheapr::which_val(GRP_sizes, 0L, invert = TRUE)] <- o[starts]
      } else {
        out <- o[starts]
      }
    }
  }
  if (is.null(out)){
    out <- integer()
  }
  if (use.g.names){
    names(out) <- GRP_names(GRP)
  }
  out
}
# Extract group ends from GRP object safely and efficiently
GRP_ends <- function(GRP, use.g.names = FALSE,
                     loc = NULL){
  GRP_sizes <- GRP_group_sizes(GRP)
  if (GRP_is_sorted(GRP)){
    out <- sorted_group_ends(GRP_sizes)
    # For factors with 0 size, replace 0 with NA
    out[cheapr::which_val(GRP_sizes, 0L)] <- 0L
  } else {
    if (is.null(loc)){
      loc <- GRP_loc(GRP, use.g.names = FALSE)
    }
    out <- GRP_loc_ends(loc, GRP_sizes)
  }
  if (is.null(out)){
    out <- integer()
  }
  if (use.g.names){
    names(out) <- GRP_names(GRP)
  }
  out
}
# Extract group order from GRP object safely
GRP_order <- function(GRP){
  ### Only use the below arguments
  ### If GRP_order is called from radixorderv2
  ### Otherwise leave as is
  # starts = TRUE, group.sizes = FALSE, sort = TRUE){
  if (is.null(GRP)){
    return(NULL)
  }
  out <- GRP[["order"]]
  if (is.null(out)){
    group_id <- GRP_group_id(GRP)
    if (GRP_is_sorted(GRP) || cpp_group_id_sorted(group_id)){
      out <- seq_along(group_id)
      sizes <- GRP_group_sizes(GRP)
      starts <- GRP_starts(GRP)
      attributes(out) <- list(starts = starts,
                              maxgrpn = collapse::fmax(sizes),
                              sorted = TRUE)
      # }
    } else {
      out <- collapse::radixorderv(group_id,
                                   starts = TRUE,
                                   group.sizes = FALSE,
                                   sort = TRUE)
    }
  }
  out
}

# Alternative to gsplit(NULL, g)
GRP_loc <- function(GRP, use.g.names = FALSE){

  locs <- GRP[["locs"]]
  if (!is.null(locs)){
    return(locs)
  }
  group_id <- GRP_group_id(GRP)
  group_sizes <- GRP_group_sizes(GRP)
  group_order <- GRP[["order"]]
  if (!is.null(group_order)){
    out <- cpp_group_locs(group_order, group_sizes)
    if (use.g.names){
      names(out) <- GRP_names(GRP)
    }
  } else if (length(group_id) == 0L){
    if (use.g.names){
      out <- add_names(list(), character(0))
    } else {
      out <- list()
    }
  } else {
    out <- cpp_group_locs2(group_id, group_sizes)
    # out <- collapse::gsplit(NULL, g = GRP, use.g.names = use.g.names)
  }
  out
}

group_locs <- function(x){
  if (is_GRP(x)){
    GRP_loc(x)
  } else {
    # g <- group3(x, group.sizes = TRUE)
    # cpp_group_locs2(g, attr(g, "group.sizes", TRUE))
    o <- radixorderv2(x, group.sizes = TRUE, starts = FALSE, sort = TRUE)
    cpp_group_locs(o, attr(o, "group.sizes"))
  }
}
# GRP starts & ends from list of group locations
# Groups are assumed to be sorted and
# index locations are also assumed to be sorted
GRP_loc_starts <- function(loc){
  cpp_list_subset(loc, integer(), 1L, 0L)
}
GRP_loc_ends <- function(loc, sizes = NULL){
  if (is.null(sizes)){
    sizes <- cheapr::list_lengths(loc)
  }
  list_subset(loc, sizes, default = 0L)
}
GRP_ordered <- function(GRP){
  GRP[["ordered"]]
}
GRP_is_ordered <- function(GRP){
  ordered <- GRP_ordered(GRP)
  sorted <- ordered[names(ordered) == "sorted"]
  ordered <- ordered[names(ordered) == "ordered"]
  isTRUE(ordered || (is.na(ordered) && !is.na(sorted)))
}
# Logical is GRP sorted
GRP_is_sorted <- function(GRP){
  ordered <- GRP_ordered(GRP)
  isTRUE(ordered[names(ordered) == "sorted"])
}
GRP_group_data <- function(GRP, expand = FALSE){
  out <- list_as_tbl(as.list(GRP_groups(GRP)))
  if (expand){
    out <- cheapr::sset_df(out, GRP_group_id(GRP))
  }
  out
}

GRP_names <- function(GRP, sep = "_", expand = FALSE, force.char = FALSE){
  g_names <- collapse::GRPnames(GRP, force.char = force.char, sep = sep)
  if (expand && !is.null(g_names)){
    g_names[GRP_group_id(GRP)]
  } else {
    g_names
  }
}
# Convert data frame to GRP safely
# Either treats data as 1 big group or
# Uses dplyr group vars
grouped_df_as_GRP <- function(data, return.order = TRUE, ...){
  cpp_grouped_df_as_grp(data)
}
# Custom GRP method for data frames
# Group starts is always returned
df_to_GRP <- function(data, .cols = character(),
                      order = group_by_order_default(data),
                      # drop = df_group_by_drop_default(data),
                      return.order = TRUE,
                      return.groups = TRUE,
                      return.locs = FALSE){
  dplyr_groups <- group_vars(data)
  cols <- drop_names(col_select_names(data, .cols = .cols))
  extra_groups <- vec_setdiff(cols, dplyr_groups)
  data2 <- cheapr::sset_df(data, j = cols)

  if (length(names(data2)) == 0L){
    out <- grouped_df_as_GRP(cpp_ungroup(data2),
                             return.groups = return.groups,
                             return.order = return.order,
                             return.locs = return.locs)
  } else if (length(extra_groups) == 0L &&
             order == group_by_order_default(data)){
             # drop == df_group_by_drop_default(data)){
    out <- grouped_df_as_GRP(data2, return.order = return.order,
                             return.groups = return.groups,
                             return.locs = return.locs)
  } else {
    data2 <- cpp_ungroup(data2)
    data3 <- df_mutate_exotic_to_ids(data2, order = order)
    out <- GRP3(
      data3, sort = order,
      return.order = return.order,
      return.groups = FALSE,
      call = FALSE
    )

    # Basically if any addresses don't match,
    # then df_mutate_exotic_to_ids() has converted some
    # cols to group id cols.
    # If this is the case we need to sset the distinct groups
    # using the original data.

    # Always add group starts as it's usually not too expensive

    starts <- GRP_starts(out)
    out[["group.starts"]] <- starts

    if (return.groups){

      if (order){
        slice <- !(length(starts) == df_nrow(data2) &&
                     isTRUE(attr(out[["order"]], "sorted")))
      } else {
        slice <- !(length(starts) == df_nrow(data2))
      }

      if (slice){
        groups <- cheapr::sset(data2, starts)
      } else {
        groups <- data2
      }
      out[["group.vars"]] <- cols
      # if (!drop){
      #   groups <- expand_unused_levels(groups)
      # Extra logic to adjust group sizes and group starts
      # }
      out[["groups"]] <- groups
    }
  }
  if (return.locs){
    out <- cheapr::list_assign(out, list("locs" = GRP_loc(out)))
  }
  class(out) <- "GRP"
  out
}
#' @exportS3Method collapse::GRP
GRP.Interval <- function(X, return.groups = TRUE, ...){
  x <- X
  X <- interval_separate(x)
  out <- collapse::GRP(X, return.groups = FALSE, ...)
  if (return.groups){
    out[["groups"]] <- list(x = x[GRP_starts(out)])
  }
  out
}

#' @exportS3Method collapse::GRP
GRP.list <- function(X, return.groups = TRUE, ...){
  out <- collapse::GRP(group_id(X, as_qg = TRUE), return.groups = FALSE, ...)
  if (return.groups){
    out[["groups"]] <- list(x = cheapr::sset(X, GRP_starts(out)))
  }
  out
}
#' @exportS3Method collapse::GRP
GRP.vctrs_rcrd <- function(X, return.groups = TRUE, ...){
  out <- GRP2(cheapr::list_as_df(X), return.groups = FALSE, ...)
  if (return.groups){
    out[["groups"]] <- list(x = cheapr::sset(X, GRP_starts(out)))
  }
  out[[10L]] <- NULL
  out
}

#' @exportS3Method collapse::GRP
GRP.NULL <- function(X, ...){
  NULL
}
#' @exportS3Method collapse::GRP
GRP.integer64 <- function(X, return.groups = TRUE, ...){
  out <- collapse::GRP(cpp_int64_to_numeric(X),  return.groups = FALSE, ...)
  if (return.groups){
    out[["groups"]] <- list(x = cheapr::sset(X, GRP_starts(out)))
  }
  out
}
gsplit2 <- function(x = NULL, g = NULL, use.g.names = FALSE, ...){
  if (is.null(g)){
    if (is.null(x)){
      list(integer())
    } else {
      list(x)
    }
  } else {
    collapse::gsplit(x, g = g, use.g.names = use.g.names, ...)
  }
}

radixorderv2 <- function(x, na.last = TRUE, decreasing = FALSE,
                         starts = FALSE, sort = TRUE, group.sizes = FALSE){
  if (is.null(x)){
    return(NULL)
  }
  if (is_GRP(x)){
    return(GRP_order(x))
  }
  if (is_df(x)){
    if (df_ncol(x) == 0){
      N <- df_nrow(x)
      out <- seq_len(N)
      if (starts){
        attr(out, "starts") <- if (N == 0) integer() else 1L
      }
      if (group.sizes){
        attr(out, "group.sizes") <- if (N == 0) integer() else N
      }
      attr(out, "maxgrpn") <- N
      attr(out, "sorted") <- TRUE
      return(out)
    } else {
      x <- df_mutate_exotic_to_ids(cpp_ungroup(x), order = TRUE)
    }
  } else if (cpp_is_exotic(x)){
    x <- group_id(x, order = TRUE)
  }
  collapse::radixorderv(x, starts = starts, sort = sort, group.sizes = group.sizes,
                        na.last = na.last, decreasing = decreasing)
}

# Helper to grab group sizes
group_sizes <- function(x, sort = FALSE, expand = FALSE){
  if (sort && !expand){
    groups <- radixorderv2(x, group.sizes = TRUE, sort = TRUE)
  } else {
    groups <- group2(x)
  }
  out <- attr(groups, "group.sizes")
  if (expand){
    out <- out[groups]
  }
  out
}

## Construct a grouped data frame from a GRP object

construct_grouped_df <- function(data, g, group_vars){

  if (is.null(g) || length(group_vars) == 0){
    return(f_ungroup(data))
  }

  groups <- GRP_groups(g)

  if (is.null(groups)){
    groups <- cheapr::sset_df(cpp_ungroup(data), GRP_starts(g), j = group_vars)
  }
  group_locs <- GRP_loc(g)
  groups[[".rows"]] <- vctrs_new_list_of(group_locs, integer())
  attr(groups, ".drop") <- df_group_by_drop_default(data)
  attr(groups, "ordered") <- group_by_order_default(data)
  out <- data
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}

GRP_names <- function(GRP, sep = "_", expand = FALSE, force.char = FALSE){
  g_names <- collapse::GRPnames(GRP, force.char = force.char,
                                sep = sep)
  if (expand && !is.null(g_names)) {
    g_names[GRP_group_id(GRP)]
  }
  else {
    g_names
  }
}


group_order_and_counts <- function(g = NULL){
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)) {
    sizes <- cheapr::val_rm(GRP_group_sizes(g), 0L)
  }
  else {
    sizes <- attr(o, "group.sizes")
  }
  list(order = o, sizes = sizes)
}

grouped_first <- function(x, na.rm = TRUE, g = NULL, TRA = NULL, use.g.names = FALSE){
  if (is.null(g)){
    first <- cheapr::sset(x, min(1L, vector_length(x)))
    if (na.rm && vector_length(first) != 0 && cheapr::is_na(first)){
      not_na_locs <- cheapr::na_find(x, invert = TRUE)
      first_not_na <- not_na_locs[min(1L, length(not_na_locs))]
      first <- cheapr::sset(x, first_not_na)
    }
    return(first)
  }
  if (cpp_is_exotic(x)){
    .g <- GRP2(x, sort = FALSE, return.order = FALSE, return.groups = FALSE, return.locs = FALSE)
    group_id <- GRP_group_id(.g)
    starts <- GRP_starts(.g)
    if (na.rm){
     group_id[cheapr::na_find(x)] <- NA_integer_
    }
    out <- collapse::ffirst(
      group_id, na.rm = na.rm, g = g, TRA = TRA, use.g.names = use.g.names
    )
    cheapr::sset(cheapr::sset(x, starts), out)
  } else {
    collapse::ffirst(x, na.rm = na.rm, g = g, TRA = TRA, use.g.names = use.g.names)
  }
}

grouped_last <- function(x, na.rm = TRUE, g = NULL, TRA = NULL, use.g.names = FALSE){
  if (is.null(g)){
    last <- cheapr::sset(x, vector_length(x))
    if (na.rm && vector_length(last) != 0 && cheapr::is_na(last)){
      not_na_locs <- cheapr::na_find(x, invert = TRUE)
      last_not_na <- not_na_locs[length(not_na_locs)]
      last <- cheapr::sset(x, last_not_na)
    }
    return(last)
  }
  if (cpp_is_exotic(x)){
    .g <- GRP2(x, sort = FALSE, return.order = FALSE, return.groups = FALSE, return.locs = FALSE)
    group_id <- GRP_group_id(.g)
    starts <- GRP_starts(.g)
    if (na.rm){
      group_id[cheapr::na_find(x)] <- NA_integer_
    }
    out <- collapse::flast(
      group_id, na.rm = na.rm, g = g, TRA = TRA, use.g.names = use.g.names
    )
    cheapr::sset(cheapr::sset(x, starts), out)
  } else {
    collapse::flast(x, na.rm = na.rm, g = g, TRA = TRA, use.g.names = use.g.names)
  }
}

grouped_lag <- function(x, n = 1L, fill = NULL, g = NULL){
  order_counts <- group_order_and_counts(g)
  o <- order_counts[["order"]]
  rl <- order_counts[["sizes"]]
  exotic <- cpp_is_exotic(x) && !inherits(x, "vctrs_rcrd") && !rlang::is_bare_list(x)
  y <- x
  .fill <- fill
  if (exotic){
    xg <- GRP2(y, sort = FALSE, return.groups = FALSE, return.order = FALSE)
    y <- GRP_group_id(xg)
    if (!is.null(fill)){
      fill <- GRP_n_groups(xg) + 1L
    }
  }
  if (is.null(o) && is.null(rl) && length(n) == 1L) {
    out <- cheapr::lag_(y, n, fill = fill, recursive = inherits(x, "vctrs_rcrd"))
  }
  else {
    out <- cheapr::lag2_(y, n, order = o, run_lengths = rl,
                         fill = fill, recursive = inherits(x, "vctrs_rcrd"))
  }
  if (exotic){
    uniq <- cheapr::sset(x, GRP_starts(xg))
    if (!is.null(fill)){
      uniq <- cheapr::cheapr_c(uniq, .fill)
    }
    out <- cheapr::sset(uniq, out)
  }
  out
}

grouped_lead <- function(x, n = 1L, fill = NULL, g = NULL){
  grouped_lag(x, n = -n, fill = fill, g = g)
}
