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
      raw_group_collapse(cheapr::fast_df(data = data),
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
  g <- GRP2(cpp_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = TRUE,
            return.order = order || loc,
            method = "auto",
            call = FALSE,
            .drop = .drop)
  out <- as_tbl(GRP_groups(g))
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
    out <- cheapr::sset_row(out, unsorted_i)
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
      num_missing_categories <- group_data_size -
        collapse::fnunique(
          remove_rows_if_any_na(factors)
        )
      if (num_missing_categories > 0){

        # Keeping this here in case the functionality changes in the future
        # factor_unique <- function(x){
        #   out <- seq_along(levels(x))
        #   if (cheapr::any_na(x)){
        #     out <- c(out, NA)
        #   }
        #   attributes(out) <- attributes(x)
        #   out
        # }

        full <- list_as_df(
          add_names(
            do.call(cross_join, lapply(factors, cheapr::levels_factor)),
            names(factors)
          )
        )
        # If full included combinations of NA factor values
        # via `factor_unique()` as shown above in comments
        # We could do a left join

        # out <- f_left_join(full, out, sort = (order && sort))
        # anti_matches <- which_not_in(full, factors)
        # if (id){
        #  cpp_loc_set_replace(out[[".group"]], anti_matches, NA_integer_)
        # }
        # if (loc){
        #   out[[".loc"]][anti_matches] <- list(integer())
        # }
        # if (start){
        #   cpp_loc_set_replace(out[[".start"]], anti_matches, 0L)
        # }
        # if (end){
        #   cpp_loc_set_replace(out[[".end"]], anti_matches, 0L)
        # }
        # if (size){
        #   cpp_loc_set_replace(out[[".size"]], anti_matches, 0L)
        # }

        # out <- f_full_join(full, out, sort = (order && sort))
        # if (id){
        #  cpp_loc_set_replace(out[[".group"]], cheapr::which_na(out[[".group"]]), NA_integer_)
        # }
        # if (loc){
        #   out[[".loc"]][cheapr::val_find(
        #     cheapr::list_lengths(out[[".loc"]]),
        #     0)] <- list(integer())
        # }
        # if (start){
        #   cpp_loc_set_replace(out[[".start"]], cheapr::which_na(out[[".start"]]), 0L)
        # }
        # if (end){
        #   cpp_loc_set_replace(out[[".end"]], cheapr::which_na(out[[".end"]]), 0L)
        # }
        # if (size){
        #   cpp_loc_set_replace(out[[".size"]], cheapr::which_na(out[[".size"]]), 0L)
        # }
        #
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
    out <- fast_tbl(".group" = integer(ss) + 1L)
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
    group_info <- tidy_group_info(cpp_ungroup(data), ..., .by = {{ .by }},
                                  .cols = .cols,
                                  ungroup = TRUE,
                                  rename = TRUE)
    all_groups <- group_info[["all_groups"]]
    if (length(all_groups) == 0){
      out <- group_data_collapse(
        cpp_ungroup(data), id = id, size = size, loc = loc,
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
  n_groups <- df_nrow(out)
  out_nms <- names(out)
  out <- col_rename(out, .cols = c(".loc" = ".rows"))
  locs <- out[[".loc"]]

  if (id){
    out[[".group"]] <- df_seq_along(out, "rows")
    ncol <- ncol(out)
    out <- cheapr::sset_df(out, j = c(seq_len(ncol - 2L), ncol, ncol - 1L))
  }
  if (start){
    out <- df_add_col(out, ".starts", GRP_loc_starts(locs))
  }
  if (end){
    gends <- integer(n_groups)
    gends[which(cheapr::list_lengths(locs != 0L))] <- GRP_loc_ends(locs)
    out <- df_add_col(out, ".end", gends)
  }
  if (size){
    out <- df_add_col(out, ".size", cheapr::list_lengths(locs))
  }
  if (!loc){
    out <- df_rm_cols(out, ".loc")
  }
  out
}


df_collapse <- function(data, cols = names(data),
                        order = df_group_by_order_default(data), sort = order,
                        id = FALSE, size = FALSE, loc = TRUE,
                        start = FALSE, end = FALSE,
                        drop = df_group_by_drop_default(data),
                        add = TRUE){
  if (!add){
    data <- f_ungroup(data)
  }
  g <- df_to_GRP(data, .cols = cols,
                 order = order,
                 return.groups = TRUE,
                 return.order = FALSE)
  out <- as_tbl(GRP_groups(g))
  if (id){
    out <- df_add_col(out, ".group", df_seq_along(out))
  }
  include_loc <- loc || end
  if (include_loc){
    if (add && identical(group_vars(data), cols) &&
        order == df_group_by_order_default(data) &&
        drop == df_group_by_order_default(data)){
      GRP_loc <- as.list(group_rows(data))
    } else {
      GRP_loc <- GRP_loc(g)
    }
    out <- df_add_col(out, ".loc", GRP_loc)
  } else {
    GRP_loc <- NULL
  }
  if (start){
    out <- df_add_col(out, ".start", GRP_starts(g))
  }
  if (end){
    out <- df_add_col(out, ".end", GRP_ends(g, loc = GRP_loc))
  }
  if (!loc && include_loc){
    out <- df_rm_cols(out, ".loc")
  }
  if (size){
    out <- df_add_col(out, ".size", GRP_group_sizes(g))
  }
  if (!sort && order){
    unsorted_i <- collapse::funique(GRP_group_id(g), sort = FALSE)
    out <- cheapr::sset_row(out, unsorted_i)
  }
  # Method for when not dropping unused factor levels
  # At the moment a bit convoluted
  if (!drop){
    group_names <- names(out)[!names(out) %in%
                                c(".group", ".loc", ".start", ".end", ".size")]
    group_out <- cheapr::sset_col(out, group_names)
    is_factor <- vapply(group_out, is.factor, FALSE, USE.NAMES = FALSE)
    non_factors <- cheapr::sset_col(group_out, cheapr::which_(is_factor, invert = TRUE))
    if (any(is_factor)){
      factors <- cheapr::sset_col(group_out, is_factor)
      group_data_size <- prod(
        vapply(factors, collapse::fnlevels, 0L)
      )
      num_missing_categories <- group_data_size -
        collapse::fnunique(
          remove_rows_if_any_na(factors)
        )
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
          out <- f_arrange(out, .cols = group_names)
        }
      }
    }
  }
  out
}

# Construct dplyr style group data from a data frame directly
construct_dplyr_group_data <- function(data, cols = names(data),
                                       order = df_group_by_order_default(data),
                                       drop = df_group_by_drop_default(data)){
  group_data <- df_collapse(
    cheapr::sset_col(data, cols),
    order = order,
    id = FALSE,
    loc = TRUE, sort = order,
    size = FALSE,
    start = FALSE, end = FALSE,
    drop = drop
  )
  group_data <- col_rename(group_data, c(".rows" = ".loc"))
  group_data <- df_add_col(group_data, ".rows", vctrs_new_list_of(group_data[[".rows"]], integer()))
  attr(group_data, ".drop") <- drop
  attr(group_data, "ordered") <- order
  group_data
}

GRP_collapse <- function(g,
                         id = FALSE,
                         size = FALSE, loc = TRUE,
                         start = FALSE, end = FALSE,
                         drop = df_group_by_drop_default(g[["X"]])){
  check_GRP(g)
  if (is.null(g[["groups"]])){
    cli::cli_abort("Please supply a {.cls GRP} {.arg g} with distinct groups attached")
  }
  out <- as_tbl(GRP_groups(g))
  if (id){
    out <- df_add_cols(out, list(.group = df_seq_along(out)))
  }
  include_loc <- loc || end
  if (include_loc){
    GRP_loc <- GRP_loc(g)
    out <- df_add_cols(out, list(.loc = GRP_loc))
  } else {
    GRP_loc <- NULL
  }
  if (start){
    out <- df_add_cols(out, list(.start = GRP_starts(g)))
  }
  if (end){
    out <- df_add_cols(out, list(.end = GRP_ends(g, loc = GRP_loc)))
  }
  if (!loc && include_loc){
    out <- df_add_cols(out, list(.loc = NULL))
  }
  if (size){
    out <- df_add_cols(out, list(.size = GRP_group_sizes(g)))
  }
  # Method for when not dropping unused factor levels
  # At the moment a bit convoluted
  if (!drop){
    group_names <- names(out)[!names(out) %in%
                                c(".group", ".loc", ".start", ".end", ".size")]
    group_out <- cheapr::sset_col(out, group_names)
    is_factor <- vapply(group_out, is.factor, FALSE, USE.NAMES = FALSE)
    non_factors <- cheapr::sset_col(group_out, cheapr::which_(is_factor, invert = TRUE))
    if (any(is_factor)){
      factors <- cheapr::sset_col(group_out, which(is_factor))
      group_data_size <- prod(
        vapply(factors, collapse::fnlevels, 0L)
      )
      num_missing_categories <- group_data_size -
        collapse::fnunique(
          remove_rows_if_any_na(factors)
        )
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
      }
    }
  }
  out
}

construct_dplyr_group_data2 <- function(g, drop = df_group_by_drop_default(GRP_data(g))){
  group_data <- GRP_collapse(
    g,
    id = FALSE,
    loc = TRUE,
    size = FALSE,
    start = FALSE,
    end = FALSE,
    drop = drop
  )
  group_data <- cheapr::list_assign(group_data,
    list(.rows = vctrs_new_list_of(group_data[[".loc"]], integer()),
         .loc = NULL)
  )
  group_data <- list_as_tbl(group_data)
  attr(group_data, ".drop") <- drop
  group_data
}

construct_fastplyr_group_data <- function(g, drop = df_group_by_drop_default(GRP_data(g))){

  out <- construct_dplyr_group_data2(g, drop = drop)
  attr(out, "ordered") <- df_group_by_order_default(GRP_data(g))
  out
}

construct_fastplyr_grouped_df <- function(g, drop = df_group_by_drop_default(GRP_group_data(g))){

  data <- GRP_data(g)
  group_vars <- GRP_group_vars(g)
  if (length(group_vars) == 0){
    return(f_ungroup(data))
  }
  group_data <- construct_dplyr_group_data2(g, drop = drop)
  attr(data, "groups") <- group_data
  attr(data, "GRP") <- g
  class(data) <- c("fastplyr_grouped_df", "grouped_df", "tbl_df", "tbl", "data.frame")
  data
}


construct_dplyr_grouped_df <- function(data, cols = names(data),
                                       order = df_group_by_order_default(data),
                                       drop = df_group_by_drop_default(data)){
  if (length(cols) == 0){
    return(f_ungroup(data))
  }
  group_data <- construct_dplyr_group_data(data, cols, order, drop)
  attr(data, "groups") <- group_data
  class(data) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  data
}

# group_collapse3 <- function(data, g,
#                             id = FALSE,
#                             size = TRUE, loc = TRUE,
#                             start = FALSE, end = FALSE,
#                             drop = df_group_by_drop_default(data)){
#   if (is.null(g[["groups"]])){
#     stop("Please supply a `GRP` `g` with distinct groups attached")
#   }
#   out <- df_as_tbl(GRP_groups(g))
#   if (id){
#     out <- df_add_cols(out, list(.group = df_seq_along(out)))
#   }
#   include_loc <- loc || end
#   if (include_loc){
#     GRP_loc <- GRP_loc(g)
#     out <- df_add_cols(out, list(.loc = GRP_loc))
#   } else {
#     GRP_loc <- NULL
#   }
#   if (start){
#     out <- df_add_cols(out, list(.start = GRP_starts(g)))
#   }
#   if (end){
#     out <- df_add_cols(out, list(.end = GRP_ends(g, loc = GRP_loc)))
#   }
#   if (!loc && include_loc){
#     out <- df_add_cols(out, list(.loc = NULL))
#   }
#   if (size){
#     out <- df_add_cols(out, list(.size = GRP_group_sizes(g)))
#   }
#   # Method for when not dropping unused factor levels
#   # At the moment a bit convoluted
#   if (!drop){
#     group_names <- names(out)[!names(out) %in%
#                                 c(".group", ".loc", ".start", ".end", ".size")]
#     group_out <- cheapr::sset_col(out, group_names)
#     is_factor <- vapply(group_out, is.factor, FALSE, USE.NAMES = FALSE)
#     non_factors <- cheapr::sset_col(group_out, cheapr::which_(is_factor, invert = TRUE))
#     if (any(is_factor)){
#       factors <- cheapr::sset_col(group_out, which(is_factor))
#       group_data_size <- prod(
#         vapply(factors, collapse::fnlevels, 0L)
#       )
#       num_missing_categories <- group_data_size -
#         collapse::fnunique(
#           remove_rows_if_any_na(factors)
#         )
#       if (num_missing_categories > 0){
#         full <- list_as_df(
#           add_names(
#             do.call(cross_join, lapply(factors, cheapr::levels_factor)),
#             names(factors)
#           )
#         )
#         missed <- f_anti_join(full, group_out)
#         for (non_factor in names(group_out)[cheapr::which_(is_factor, invert = TRUE)]){
#           missed[[non_factor]] <- group_out[[non_factor]][NA_integer_]
#         }
#         if (id){
#           missed[[".group"]] <- NA_integer_
#         }
#         # Bind the combinations that don't exist
#         if (loc){
#           missed[[".loc"]] <- list(integer())
#         }
#         if (start){
#           missed[[".start"]] <- 0L
#         }
#         if (end){
#           missed[[".end"]] <- 0L
#         }
#         if (size){
#           missed[[".size"]] <- 0L
#         }
#         out <- f_bind_rows(out, missed)
#         if (id){
#           out[[".group"]] <- group_id(f_select(out, .cols = group_names),
#                                       order = order)
#         }
#         out <- f_arrange(out, .cols = group_names)
#
#       }
#     }
#   }
#   df_as_tbl(out)
# }
