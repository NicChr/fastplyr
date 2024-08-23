group_collapse <- function(data, ..., order = TRUE, sort = order,
                           ascending = TRUE,
                           .by = NULL, .cols = NULL,
                           id = TRUE,
                           size = TRUE, loc = TRUE,
                           # loc_order = TRUE,
                           start = TRUE, end = TRUE,
                           .drop = df_group_by_drop_default(data)){
  UseMethod("group_collapse")
}
raw_group_collapse <- function(data, order = TRUE, sort = order,
                                   ascending = TRUE,
                                   id = TRUE,
                                   size = TRUE, loc = TRUE,
                                   start = TRUE, end = TRUE,
                                   .drop = df_group_by_drop_default(data)){
  if (is.factor(data)){
    return(
      raw_group_collapse(new_df(data = data),
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
  out <- list_as_df(as.list(GRP_groups(g)))
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
    non_factors <- f_select(group_out, .cols = which(is_factor, invert = TRUE))
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
        missed <- collapse_join(
          full, group_out, how = "anti", on = names(full)
        )
        for (non_factor in names(group_out)[which(is_factor, invert = TRUE)]){
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
        out <- collapse::rowbind(out, missed, return = "data.frame")
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
  df_as_tbl(out)
}
#' @export
group_collapse.data.frame <- function(data, ..., order = TRUE, sort = order,
                                      ascending = TRUE,
                                      .by = NULL, .cols = NULL,
                                      id = TRUE,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = TRUE, end = TRUE,
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
      out[[".loc"]] <- list(rowids)
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
group_collapse.grouped_df <- function(data, ..., order = TRUE, sort = order,
                                      ascending = TRUE,
                                      .by = NULL, .cols = NULL,
                                      id = TRUE,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = TRUE, end = TRUE,
                                      .drop = df_group_by_drop_default(data)){
  n_dots <- dots_length(...)
  # Error checking on .by
  check_by(data, .by = {{ .by }})
  # Special conditions where if met,
  # we can use dplyr grouping structure
  if (n_dots == 0 &&
      is.null(.cols) &&
      order &&
      ascending &&
      sort &&
      .drop == df_group_by_drop_default(data)){
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
  } else {
    group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                  .cols = .cols,
                                  ungroup = TRUE,
                                  rename = TRUE)
    all_groups <- group_info[["all_groups"]]
    out <- raw_group_collapse(
      f_select(group_info[["data"]], .cols = all_groups),
      order = order, sort = sort,
      id = id,
      size = size, loc = loc,
      ascending = ascending,
      # loc_order = loc_order,
      start = start, end = end,
      .drop = .drop
    )
    attr(out, ".drop") <- .drop
  }
  out
}
