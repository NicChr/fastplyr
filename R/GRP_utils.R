GRP2 <- function(X, by = NULL, sort = TRUE, return.order = sort,
                 return.groups = FALSE, ...){
  if (is_df(X)){
    if (is.null(by)){
      by <- names(X)
    }
    df_to_GRP(X, .cols = by, order = sort,
              return.order = return.order,
              return.groups = return.groups)
  } else {
    collapse::GRP(X, by = by, sort = sort, ...)
  }
}

# Two alternatives to collapse::group
# They both handle nested data frames

group2 <- function(X){
  group_id(X, order = FALSE, as_qg = TRUE)
}
group3 <- function(X, starts = FALSE, group.sizes = FALSE){
  if (is.null(X)) {
    return(NULL)
  }
  if (is_df(X)) {
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
      X <- df_mutate_exotic_to_ids(X)
    }
  }
  if (is.list(X) && length(X) == 0) {
    return(NULL)
  }
  collapse::group(X, starts = starts, group.sizes = group.sizes)
}

# Is object a collapse GRP?
is_GRP <- function(GRP){
  inherits(GRP, "GRP")
}
# Number of groups
GRP_n_groups <- function(GRP){
  .subset2(GRP, "N.groups")
  # GRP[["N.groups"]]
}
# Group IDs (integer)
GRP_group_id <- function(GRP){
  .subset2(GRP, "group.id")
  # GRP[["group.id"]]
}
GRP_data_size <- function(GRP){
  length(GRP_group_id(GRP))
}
# Group sizes
GRP_group_sizes <- function(GRP){
  # GRP[["group.sizes"]]
  .subset2(GRP, "group.sizes")
}
GRP_expanded_group_sizes <- function(GRP){
  GRP_group_sizes(GRP)[GRP_group_id(GRP)]
}
# Groups
GRP_groups <- function(GRP){
  # GRP[["groups"]]
  .subset2(GRP, "groups")
}
# Group variable names
GRP_group_vars <- function(GRP){
  .subset2(GRP, "group.vars")
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

GRP_duplicated <- function(GRP, all = FALSE){
  sizes <- GRP_group_sizes(GRP)
  group_id <- GRP_group_id(GRP)
  if (all){
    out <- (sizes > 1L)[group_id]
  } else {
    out <- row_id(GRP) > 1L
  }
  out
}
# Alternative that just returns dup indices
# The commented-out code is simpler but cheapr::which_val is more efficient
# Especially with data that contains few duplicates
# GRP_which_duplicated <- function(GRP, all = FALSE){
#   which(GRP_duplicated(GRP, all))
# }
GRP_which_duplicated <- function(GRP, all = FALSE){
  sizes <- GRP_group_sizes(GRP)
  group_id <- GRP_group_id(GRP)
  if (all){
    which((sizes > 1L)[group_id])
  } else {
    cheapr::which_val(row_id(GRP), 1L, invert = TRUE)
  }
}
calc_sorted_group_starts <- function(group_sizes, init_loc = 1L){
  cpp_sorted_group_starts(as.integer(group_sizes), init_loc)
}
calc_sorted_group_ends <- function(group_sizes){
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
        out <- calc_sorted_group_starts(GRP_sizes)
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
    out <- calc_sorted_group_ends(GRP_sizes)
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
    if (GRP_is_sorted(GRP) || is_sorted(group_id)){
      out <- seq_along(group_id)
      sizes <- GRP_group_sizes(GRP)
      if (is.null(GRP[["group.starts"]])){
        starts <- calc_sorted_group_starts(sizes)
      } else {
        starts <- GRP[["group.starts"]]
      }
      # This should not be used unless through radixorderv
      # if (group.sizes){
      #   attributes(out) <- list("starts" = GRP_starts(GRP),
      #                           "group.sizes" = GRP_group_sizes(GRP),
      #                           "maxgrpn" = collapse::fmax(GRP_group_sizes(GRP)),
      #                           "sorted" = TRUE)
      # } else {
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
  if (!is.null(GRP[["order"]])){
    out <- cpp_group_locs(GRP[["order"]], GRP[["group.sizes"]])
    if (use.g.names){
      names(out) <- GRP_names(GRP)
    }
  } else if (length(GRP_group_id(GRP)) == 0L){
    if (use.g.names){
      out <- add_names(list(), character(0))
    } else {
      out <- list()
    }
  } else {
    out <- collapse::gsplit(NULL, g = GRP, use.g.names = use.g.names)
  }
  out
}

group_locs <- function(x){
  if (is_GRP(x)){
    GRP_loc(x)
  } else {
    o <- radixorderv2(x, group.sizes = TRUE, starts = FALSE, sort = TRUE)
    cpp_group_locs(o, attr(o, "group.sizes"))
  }
}
# GRP starts & ends from list of group locations
# Groups are assumed to be sorted and
# index locations are also assumed to be sorted
GRP_loc_starts <- function(loc){
  list_subset(loc, 1L, default = 0L)
  # unlist(
  #   collapse::ffirst(
  #     loc,
  #     use.g.names = FALSE,
  #     na.rm = FALSE
  #   )
  #   , use.names = FALSE, recursive = FALSE
  # )
}
GRP_loc_ends <- function(loc, sizes = NULL){
  if (is.null(sizes)){
    sizes <- cheapr::lengths_(loc)
  }
  list_subset(loc, sizes, default = 0L)
  # unlist(
  #   collapse::flast(
  #     loc,
  #     use.g.names = FALSE,
  #     na.rm = FALSE
  #   )
  #   , use.names = FALSE, recursive = FALSE
  # )
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
    out <- df_row_slice(out, GRP_group_id(GRP))
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
df_as_GRP <- function(data, return.groups = TRUE, return.order = TRUE){
  out <- vector("list", 9L)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gdata <- group_data(data)
  gvars <- group_vars(data)
  n_groups <- df_nrow(gdata)
  group_id <- df_group_id(data)
  gsizes <- cheapr::lengths_(gdata[[".rows"]])
  if (return.order){
    gorder <- collapse::radixorderv(group_id,
                                    starts = TRUE,
                                    sort = TRUE,
                                    na.last = TRUE)
    sorted <- attr(gorder, "sorted")
  } else {
    gorder <- NULL
    sorted <- NA
  }
  gordered <- c("ordered" = TRUE,
                "sorted" = sorted)
  if (return.groups){
    gstarts <- GRP_loc_starts(gdata[[".rows"]])
  }
  out[["N.groups"]] <- n_groups
  out[["group.id"]] <- group_id
  out[["group.sizes"]] <- gsizes
  if (length(gvars) > 0L && return.groups){
    out[["groups"]] <- f_select(gdata, .cols = gvars)
    out[["group.vars"]] <- gvars
    out[["group.starts"]] <- gstarts
  }
  if (!is.null(gorder)){
    out[["order"]] <- gorder
  }
  out[["ordered"]] <- gordered
  class(out) <- "GRP"
  out
}
df_as_one_GRP <- function(data, order = TRUE,
                          return.order = TRUE){
  out <- vector("list", 9L)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gsizes <- df_nrow(data)
  n_groups <- min(gsizes, 1L)
  gstarts <- if (n_groups == 0L) NULL else n_groups
  group_id <- collapse::alloc(1L, gsizes)
  if (order && return.order){
    gorder <- seq_len(gsizes)
    sorted <- TRUE
    attributes(gorder) <- list(starts = gstarts,
                               maxgrpn = gsizes,
                               sorted = TRUE)
  } else {
    gorder <- NULL
    sorted <- NA
  }
  gordered <- c("ordered" = order,
                "sorted" = sorted)
  out[["N.groups"]] <- n_groups
  out[["group.id"]] <- group_id
  out[["group.sizes"]] <- gsizes
  if (!is.null(gorder)){
    out[["order"]] <- gorder
  }
  if (!is.null(gstarts)){
    out[["group.starts"]] <- gstarts
  }
  out[["ordered"]] <- gordered
  class(out) <- "GRP"
  out
}
# Custom GRP method for data frames
# Group starts is always returned
df_to_GRP <- function(data, .cols = character(0),
                      order = df_group_by_order_default(data),
                      return.order = order,
                      return.groups = FALSE){
  dplyr_groups <- group_vars(data)
  cols <- col_select_names(data, .cols = .cols)
  extra_groups <- setdiff(cols, dplyr_groups)
  group_vars <- c(dplyr_groups, extra_groups)
  data <- f_select(data, .cols = group_vars)

  if (length(names(data)) == 0L){
    out <- df_as_one_GRP(data, order = order, return.order = return.order)
  } else if (length(extra_groups) == 0L && order == df_group_by_order_default(data)){
    out <- df_as_GRP(data, return.order = return.order, return.groups = return.groups)
  } else {
    data <- df_ungroup(data)
    data2 <- df_mutate_exotic_to_ids(data)
    out <- collapse::GRP(data2, sort = order,
                         return.order = return.order,
                         return.groups = return.groups,
                         call = FALSE)

    # Basically if any addresses don't match,
    # then df_mutate_exotic_to_ids() has converted some
    # cols to group id cols.
    # If this is the case we need to sset the distinct groups
    # using the original data.

    if (return.groups && !all(cpp_address_equal(data, data2))){
      if (return.groups){
        out[["groups"]] <- cheapr::sset(data, GRP_starts(out))
      }
    }
  }
  out
}
#' @exportS3Method collapse::GRP
GRP.Interval <- function(X, ...){
  x <- X
  X <- interval_separate(x)
  out <- collapse::GRP(X, ...)
  if (!is.null(GRP_groups(out))){
    out[["groups"]] <- list(x = x[GRP_starts(out)])
  }
  out
}
#' @exportS3Method collapse::GRP
GRP.NULL <- function(X, ...){
  NULL
}
# Use this to turn a sorted group ID into a GRP when you have basic group information
sorted_group_id_to_GRP <- function(x,
                                   n_groups,
                                   group_sizes,
                                   group.starts = TRUE,
                                   groups = NULL,
                                   group.vars = NULL){
  out <- structure(
    list(
      "N.groups" = n_groups,
      "group.id" = x,
      "group.sizes" = group_sizes,
      "groups" = groups,
      "group.vars" = group.vars,
      "ordered" = c("ordered" = TRUE, "sorted" = TRUE),
      "order" = seq_along(x),
      "group.starts" = NULL,
      "call" = NULL
    ),
    class = "GRP"
  )
  gstarts <- calc_sorted_group_starts(group_sizes)
  if (group.starts){
    out[["group.starts"]] <- gstarts
  }
  attributes(out[["order"]]) <- list(starts = gstarts,
                                     maxgrpn = collapse::fmax(group_sizes),
                                     sorted = TRUE)
  # Alternative way of getting group starts is:
  # c(1L, which(x) != collapse::flag(x))
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
# Sorts data by groups and returns key info
sort_data_by_GRP <- function(x, g, sorted_group_starts = TRUE){
  has_groups <- !is.null(g)
  if (!has_groups){
    return(list(
      x = x,
      n_groups = min(NROW(x), 1L),
      group_sizes = NROW(x),
      GRP = NULL,
      sorted_GRP = NULL,
      sorted = TRUE,
      group_order = NULL,
      has_groups = FALSE
    ))
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  group_id <- GRP_group_id(g)
  group_sizes <- GRP_group_sizes(g)
  n_groups <- GRP_n_groups(g)
  group_order <- GRP_order(g)
  groups_are_sorted <- isTRUE(attr(group_order, "sorted"))
  if (!groups_are_sorted){
    x <- cheapr::sset(x, group_order)
    group_id <- cheapr::sset(group_id, group_order)
  }
  if (sorted_group_starts){
    if (groups_are_sorted){
      sorted_group_starts <- GRP_starts(g)
    } else {
      sorted_group_starts <- calc_sorted_group_starts(group_sizes)
    }
  } else {
    sorted_group_starts <- NULL
  }
  sorted_GRP <- sorted_group_id_to_GRP(
    group_id,
    n_groups = n_groups,
    group_sizes = group_sizes,
    group.starts = FALSE
  )
  sorted_GRP[["group.starts"]] <- sorted_group_starts
  list(x = x,
       n_groups = n_groups,
       group_sizes = group_sizes,
       GRP = g,
       sorted_GRP = sorted_GRP,
       sorted = groups_are_sorted,
       group_order = group_order,
       has_groups = has_groups)

}
# greorder but x can be a data frame or list
greorder2 <- function(x, g, ...){
  if (is.null(g)){
    return(x)
  }
  if (is.list(x)){
    cheapr::sset(
      x,
      collapse::greorder(
        seq_len(NROW(x)), g = g, ...
      )
    )
  } else {
    collapse::greorder(x, g = g, ...)
  }
}

radixorderv2 <- function(x, na.last = TRUE, decreasing = FALSE,
                         starts = FALSE, sort = TRUE, group.sizes = FALSE){
  if (is.null(x)){
    return(NULL)
  }
  if (is_df(x) && df_ncol(x) == 0){
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
  }
  if (is_GRP(x)){
    return(GRP_order(x))
  }
  if (is_df(x)){
    x <- df_mutate_exotic_to_ids(df_ungroup(x))
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
new_GRP <- function(N.groups = NULL,
                    group.id = NULL,
                    group.sizes = NULL,
                    groups = NULL,
                    group.vars = NULL,
                    ordered = NULL,
                    order = NULL,
                    group.starts = NULL,
                    call = NULL){
  out <- list(
    N.groups = N.groups,
    group.id = group.id,
    group.sizes = group.sizes,
    groups = groups,
    group.vars = group.vars,
    ordered = ordered,
    order = order,
    group.starts = group.starts,
    call = call
  )
  class(out) <- "GRP"
  out
}

# A wrapper to grab the order and counts of groups
# Specifically for the c++ rolling calculations
group_order_and_counts <- function(g = NULL){
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)){
    # Accounting for factors
    sizes <- val_rm(GRP_group_sizes(g), 0L)
  } else {
    sizes <- attr(o, "group.sizes")
  }
  list(order = o, sizes = sizes)
}
