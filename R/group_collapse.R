df_collapse <- function(data, cols = names(data),
                        order = group_by_order_default(data), sort = order,
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
                 return.order = order)
  out <- as_tbl(GRP_groups(g))
  if (id){
    out <- df_add_col(out, ".group", df_seq_along(out))
  }
  include_loc <- loc || end
  if (include_loc){
    if (add && identical(group_vars(data), cols) &&
        order == group_by_order_default(data) &&
        drop == group_by_order_default(data)){
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
        full <- cheapr::list_as_df(
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

# Construct dplyr style group data from a data frame directly
# construct_dplyr_group_data <- function(data, cols = names(data),
#                                        order = group_by_order_default(data),
#                                        drop = df_group_by_drop_default(data)){
#   group_data <- df_collapse(
#     cheapr::sset_col(data, cols),
#     order = order,
#     id = FALSE,
#     loc = TRUE, sort = order,
#     size = FALSE,
#     start = FALSE, end = FALSE,
#     drop = drop
#   )
#   group_data <- col_rename(group_data, c(".rows" = ".loc"))
#   group_data <- df_add_col(group_data, ".rows", vctrs_new_list_of(group_data[[".rows"]], integer()))
#   attr(group_data, ".drop") <- drop
#   attr(group_data, "ordered") <- order
#   group_data
# }

GRP_collapse <- function(g,
                         id = FALSE,
                         size = FALSE, loc = TRUE,
                         start = FALSE, end = FALSE,
                         drop){
  check_GRP(g)
  if (is.null(g[["groups"]])){
    cli::cli_abort("Please supply a {.cls GRP} {.arg g} with distinct groups attached")
  }
  out <- as_tbl(GRP_groups(g))
  if (id){
    out <- cheapr::df_modify(out, list(.group = df_seq_along(out)))
  }
  include_loc <- loc || end
  if (include_loc){
    GRP_loc <- GRP_loc(g)
    out <- cheapr::df_modify(out, list(.loc = GRP_loc))
  } else {
    GRP_loc <- NULL
  }
  if (start){
    out <- cheapr::df_modify(out, list(.start = GRP_starts(g)))
  }
  if (end){
    out <- cheapr::df_modify(out, list(.end = GRP_ends(g, loc = GRP_loc)))
  }
  if (!loc && include_loc){
    out <- cheapr::df_modify(out, list(.loc = NULL))
  }
  if (size){
    out <- cheapr::df_modify(out, list(.size = GRP_group_sizes(g)))
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
        full <- cheapr::list_as_df(
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

construct_dplyr_group_data <- function(g, drop){
  group_data <- GRP_collapse(
    g,
    id = FALSE,
    loc = TRUE,
    size = FALSE,
    start = FALSE,
    end = FALSE,
    drop = drop
  )
  rows <- as_list_of_ints(group_data[[".loc"]])
  # Remove .loc_ptrs from rows in-place
  # remove_loc_ptrs_in_place(rows)

  group_data <- cheapr::list_assign(group_data,
    list(.rows = rows, .loc = NULL)
  )
  group_data <- list_as_tbl(group_data)
  attr(group_data, ".drop") <- drop
  group_data
}

construct_fastplyr_group_data <- function(g, drop){

  out <- construct_dplyr_group_data(g, drop = drop)
  attr(out, "ordered") <- GRP_is_ordered(g)
  # rows <- out[[".rows"]]
  # loc_ptrs <- attr(rows, ".loc_ptrs", TRUE)
  # out <- cheapr::attrs_add(
  #   out,
  #   ordered = GRP_is_ordered(g),
  #   .loc_ptrs = loc_ptrs
  # )
  # # Remove .loc_ptrs from rows in-place
  # remove_loc_ptrs_in_place(rows)
  out
}

construct_fastplyr_grouped_df <- function(data, g, drop = df_group_by_drop_default(data)){

  group_vars <- GRP_group_vars(g)
  if (length(group_vars) == 0){
    return(f_ungroup(data))
  }
  group_data <- construct_fastplyr_group_data(g, drop = drop)
  attr(data, "groups") <- group_data
  attr(data, "GRP") <- g
  class(data) <- c("fastplyr_grouped_df", "grouped_df", "tbl_df", "tbl", "data.frame")
  data
}
