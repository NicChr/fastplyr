group_collapse <- function(data, ..., order = df_group_by_order_default(data),
                           sort = order, .add = TRUE,
                           ascending = TRUE,
                           .by = NULL, .cols = NULL,
                           id = FALSE,
                           size = TRUE, loc = TRUE,
                           # loc_order = TRUE,
                           start = FALSE, end = FALSE,
                           .drop = df_group_by_drop_default(data)){
  UseMethod("group_collapse")
}
raw_group_collapse <- function(data, order = TRUE, sort = order,
                                   ascending = TRUE,
                                   id = FALSE,
                                   size = TRUE, loc = TRUE,
                                   start = FALSE, end = FALSE,
                                   .drop = df_group_by_drop_default(data)){
  if (is.factor(data)){
    return(
      raw_group_collapse(cheapr::new_df(data = data),
                         order = order,
                         sort = sort,
                         id = id,
                         size = size,
                         loc = loc,
                         start = start,
                         end = end,
                         .drop = .drop)
    )
  }
  g <- GRP2(df_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = TRUE,
            return.order = order || loc,
            method = "auto",
            call = FALSE,
            .drop = .drop)
  out <- list_as_tbl(as.list(GRP_groups(g)))
  if (id){
    out[[".group"]] <- df_seq_along(out)
  }
  include_loc <- loc || end
  if (include_loc){
    GRP_loc <- GRP_loc(g)
    out[[".loc"]] <- GRP_loc
  } else {
    GRP_loc <- NULL
  }
  if (start){
    out[[".start"]] <- GRP_starts(g)
  }
  if (end){
    out[[".end"]] <- GRP_ends(g, loc = GRP_loc)
  }
  if (!loc && include_loc){
    out[[".loc"]] <- NULL
  }
  if (size){
    out[[".size"]] <- GRP_group_sizes(g)
  }
  if (!sort && order){
    unsorted_i <- collapse::funique(GRP_group_id(g), sort = FALSE)
    out <- df_row_slice(out, unsorted_i, reconstruct = FALSE)
  }
  # Method for when not dropping unused factor levels
  # At the moment a bit convoluted
  if (!.drop){
    group_names <- names(out)[!names(out) %in%
                                c(".group", ".loc", ".start", ".end", ".size")]
    group_out <- f_select(out, .cols = group_names)
    is_factor <- vapply(group_out, is.factor, FALSE, USE.NAMES = FALSE)
    non_factors <- f_select(group_out, .cols = cheapr::which_(is_factor, invert = TRUE))
    if (any(is_factor)){
      factors <- f_select(group_out, .cols = which(is_factor))
      group_data_size <- prod(
        vapply(factors, collapse::fnlevels, 0L)
      )
      num_missing_categories <- group_data_size - collapse::fnunique(factors)
      if (num_missing_categories > 0){
        full <- list_as_df(
          add_names(
            do.call(cross_join, lapply(factors, cheapr::levels_factor)),
            names(factors)
          )
        )
        missed <- f_anti_join(full, group_out)
        for (non_factor in names(group_out)[cheapr::which_(is_factor, invert = TRUE)]){
          missed[[non_factor]] <- group_out[[non_factor]][NA_integer_]
        }
        if (id){
          missed[[".group"]] <- NA_integer_
        }
        # Bind the combinations that don't exist
        if (loc){
          missed[[".loc"]] <- list(integer())
        }
        if (start){
          missed[[".start"]] <- 0L
        }
        if (end){
          missed[[".end"]] <- 0L
        }
        if (size){
          missed[[".size"]] <- 0L
        }
        out <- f_bind_rows(out, missed)
        if (id){
          out[[".group"]] <- group_id(f_select(out, .cols = group_names),
                                      order = order)
        }
        if (order && sort){
          if (ascending){
            out <- f_arrange(out, .cols = group_names)
          } else {
            out <- f_arrange(out, dplyr::across(dplyr::all_of(group_names), desc))
          }
        }
      }
    }
  }
  out <- df_as_tbl(out)
  # attr(out, "group_id") <- GRP_group_id(g)
  out
}
#' @export
group_collapse.data.frame <- function(data, ..., order = df_group_by_order_default(data),
                                      sort = order,
                                      .add = TRUE,
                                      ascending = TRUE,
                                      .by = NULL, .cols = NULL,
                                      id = FALSE,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = FALSE, end = FALSE,
                                      .drop = df_group_by_drop_default(data)){
  N <- df_nrow(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  data <- group_info[["data"]]
  vars <- group_info[["all_groups"]]
  if (length(vars) == 0L){
    rowids <- seq_len(N)
    ss <- min(N, 1L)
    rowids <- list(rowids)[ss]
    out <- new_tbl(".group" = integer(ss) + 1L)
    if (loc){
      out[[".loc"]] <- rowids
    }
    if (start){
      out[[".start"]] <- integer(ss) + 1L
    }
    if (end){
      out[[".end"]] <- integer(ss) + N
    }
    if (size){
      out[[".size"]] <- N[ss]
    }
    if (!id){
      out[[".group"]] <- NULL
    }
  } else {
    out <- raw_group_collapse(
      f_select(data, .cols = vars),
      order = order, sort = sort,
      id = id,
      size = size, loc = loc,
      ascending = ascending,
      start = start, end = end,
      .drop = .drop
    )
  }
  out
}
#' @export
group_collapse.grouped_df <- function(data, ..., order = df_group_by_order_default(data),
                                      .add = TRUE, sort = order,
                                      ascending = TRUE,
                                      .by = NULL, .cols = NULL,
                                      id = FALSE,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = FALSE, end = FALSE,
                                      .drop = df_group_by_drop_default(data)){
  n_dots <- dots_length(...)
  check_by(data, .by = {{ .by }})
  init_group_vars <- group_vars(data)

  # Special conditions where if met,
  # we can use dplyr grouping structure

  order_same <- order == df_group_by_order_default(data)
  drop_same <- .drop == df_group_by_drop_default(data)
  no_dot_cols <- is.null(.cols)
  sort_same <- sort == order
  no_dots <- n_dots == 0

  # When defaults are unchanged we can use dplyr groups directly

  defaults_unchanged <- no_dots && no_dot_cols && order_same && drop_same && sort_same && ascending && .add == TRUE

  if (defaults_unchanged){
    out <- group_data_collapse(data, id = id, size = size, loc = loc,
                               start = start, end = start)
  } else if (.add) {
    group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                  .cols = .cols,
                                  ungroup = TRUE,
                                  rename = TRUE)
    extra_groups <- group_info[["extra_groups"]]
    all_groups <- group_info[["all_groups"]]
    groups_unchanged <- !group_info[["groups_changed"]]

    # When .add is true we only recalculate the groups when there truly are
    # extra groups being supplied or when the group data has been altered
    # through data masking

    # Same procedure as above..

    if (.add && groups_unchanged && length(extra_groups) == 0){
      out <- group_data_collapse(data, id = id, size = size, loc = loc,
                                 start = start, end = start)
    } else {
      out <- raw_group_collapse(
        f_select(group_info[["data"]], .cols = all_groups),
        order = order, sort = sort,
        id = id,
        size = size, loc = loc,
        ascending = ascending,
        start = start, end = end,
        .drop = .drop
      )
      attr(out, ".drop") <- .drop
    }
  } else {
    group_info <- tidy_group_info(df_ungroup(data), ..., .by = {{ .by }},
                                  .cols = .cols,
                                  ungroup = TRUE,
                                  rename = TRUE)
    all_groups <- group_info[["all_groups"]]
    if (length(all_groups) == 0){
      out <- group_data_collapse(
        df_ungroup(data), id = id, size = size, loc = loc,
        start = start, end = start
      )
    } else {
      out <- raw_group_collapse(
        f_select(group_info[["data"]], .cols = all_groups),
        order = order, sort = sort,
        id = id,
        size = size, loc = loc,
        ascending = ascending,
        start = start, end = end,
        .drop = .drop
      )
    }
    attr(out, ".drop") <- .drop
  }
  out
}

# Take dplyr::group_data(data)
# And add additional information basically

group_data_collapse <- function(data, size = TRUE, loc = TRUE,
                                id = FALSE, start = FALSE, end = FALSE){
  out <- group_data(data)
  out_nms <- names(out)
  out <- f_rename(out, .cols = c(".loc" = ".rows"))

  if (id){
    out[[".group"]] <- df_seq_along(out, "rows")
    ncol <- ncol(out)
    out <- f_select(out, .cols = c(seq_len(ncol - 2L), ncol, ncol - 1L))
  }
  sizes <- cheapr::lengths_(out[[".loc"]])
  if (start){
    gstarts <- GRP_loc_starts(out[[".loc"]])
    out[[".start"]] <- gstarts
  }
  if (end){
    gends <- integer(length(sizes))
    gends[which(sizes != 0L)] <- GRP_loc_ends(out[[".loc"]])
    out[[".end"]] <- gends
  }
  if (size){
    out[[".size"]] <- sizes
  }
  if (!loc){
    out[[".loc"]] <- NULL
  }
  out
}
