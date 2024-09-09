#' Fast group and row IDs
#'
#' @description
#' These are tidy-based functions for calculating group IDs and row IDs. \cr
#'
#' *  `group_id()` returns an integer vector of group IDs
#' the same size as the data.
#' *  `row_id()` returns an integer vector of row IDs.
#'
#' The `add_` variants add a column of group IDs/row IDs.
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be
#' ordered but not sorted.\cr
#'
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param ascending Should the group order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `row_id()` this determines if the row IDs are increasing or decreasing. \cr
#' \bold{NOTE} - When `order = FALSE`, the `ascending` argument is
#' ignored. This is something that will be fixed in a later version.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .name Name of the added ID column which should be a
#' character vector of length 1.
#' If `.name = NULL` (the default),
#' `add_group_id()` will add a column named "group_id",
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#'
#' @returns
#' An integer vector.
#'
#' @rdname group_id
#' @export
group_id <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){

  # Use hashing for S4 objects and non-data frame lists

  if (!is_df(data) && cpp_is_exotic(data)){
    return(
      group_id_hash(
        data, order = order, ascending = ascending, as_qg = as_qg
      )
    )
  }

  g <- GRP2(df_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = FALSE,
            return.order = order,
            method = "auto",
            call = FALSE)
  out <- GRP_group_id(g)
  if (as_qg){
    out <- group_id_to_qg(out, n_groups = GRP_n_groups(g),
                          group_starts = GRP_starts(g),
                          group_sizes = GRP_group_sizes(g),
                          ordered = order)
  }
  out
}
#' @export
group_id.factor <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  out <- unclass(data)
  if (order && ascending && !as_qg){
    out <- strip_attrs(out)
    out[cheapr::which_na(out)] <- length(levels(data)) + 1L
  } else {
    out <- group_id(out, order = order,
                    ascending = ascending, as_qg = as_qg)
  }
  out
}
#' @export
group_id.list <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_id_hash(as.list(data), order = FALSE, as_qg = as_qg)
}
group_id_hash <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){

  # vctrs internal hashing on lists is very good

  out <- as.integer(vctrs::vec_group_id(data))
  if (as_qg){
    out <- group_id(out, order = FALSE, ascending = ascending, as_qg = as_qg)
  }
  out

  # 4 Alternate methods
  # apply group_id(, order = F) on the below

  # xxhash64 on serialized data
  # hash_serialized_list(
  #   lapply(data, serialize, NULL, FALSE), 287572358
  # )

  ## xxh3 also using R's native serialization
  # xxhash_r_list_(data)

  # Using R match + unique
  # factor(data, levels = unique(data))

  # rlang's xxhash128
  # vapply(data, rlang::hash, "", USE.NAMES = FALSE)
}
# No need to have this anymore as there is a collapse::GRP.interval method..
#' @export
group_id.Interval <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  X <- interval_separate(data)
  groups <- collapse::GRP(X, sort = order,
                          decreasing = !ascending,
                          call = FALSE,
                          return.groups = FALSE,
                          return.order = order)
  out <- GRP_group_id(groups)
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = GRP_n_groups(groups),
                          group_starts = GRP_starts(groups),
                          group_sizes = GRP_group_sizes(groups),
                          ordered = order)
  }
  out
}
#' @export
group_id.GRP <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_ids <- GRP_group_id(data)
  if (!order && GRP_is_ordered(data)){
    out <- group_id(group_ids, order = order, as_qg = as_qg)
  } else {
    out <- group_ids
    if (as_qg){
      out <- group_id_to_qg(out,
                            n_groups = GRP_n_groups(data),
                            group_sizes = GRP_group_sizes(data),
                            group_starts = GRP_starts(data),
                            ordered = order)
    }
  }
  out
}
#' @export
group_id.vctrs_rcrd <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_id(
    list_as_df(data),
    order = order,
    ascending = ascending,
    as_qg = as_qg
  )

}
#' @export
group_id.NULL <- function(data, order = TRUE, ascending = TRUE, as_qg = FALSE){
  NULL
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  UseMethod("add_group_id")
}
#' @rdname group_id
#' @export
add_group_id.data.frame <- function(data, ...,
                                    order = df_group_by_order_default(data),
                                    ascending = TRUE,
                                    .by = NULL, .cols = NULL,
                                    .name = NULL,
                                    as_qg = FALSE){
  if (is.null(.name)){
    .name <- unique_col_name(names(data), "group_id")
  }
  N <- df_nrow(data)
  check_by(data, .by = {{ .by }})
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  group_vars <- group_info[["dplyr_groups"]]
  groups_changed <- group_info[["groups_changed"]]

  # Usual Method for when data does not contain interval
  if (length(all_groups) == 0L){
    ids <- rep_len(1L, N)
    n_groups <- min(N, 1L)
    group_sizes <- N
    group_starts <- n_groups
    if (as_qg){
      ids <- group_id_to_qg(ids,
                            n_groups = n_groups,
                            group_sizes = group_sizes,
                            group_starts = group_starts,
                            ordered = order)
    }
  } else {
    if (length(extra_groups) == 0 &&
        length(group_vars) == length(group_vars(data)) &&
        !groups_changed &&
        order == df_group_by_order_default(data) &&
        ascending &&
        !as_qg){
      ids <- df_group_id(data)
    } else {
      ids <- group_id(
        f_select(group_info[["data"]], .cols = all_groups),
        order = order,
        ascending = ascending,
        as_qg = as_qg
      )
    }
  }
  col_to_add <- add_names(list(ids), .name)
  out <- dplyr::dplyr_col_modify(df_ungroup(data), col_to_add)
  reconstruct(data, out)
}
#' @rdname group_id
#' @export
row_id <- function(data, ascending = TRUE){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(data, ascending = TRUE){
  o <- radixorderv2(data, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is.null(o)){
    return(seq_len(NROW(data)))
  }
  # Basically the order item of a GRP object
  # Doesn't naturally come with group sizes
  group_sizes <- attr(o, "group.sizes")
  starts <- attr(o, "starts")
  is_sorted <- isTRUE(attr(o, "sorted"))
  if (is_sorted){
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequence(group_sizes, from = start, by = every)
  } else {
    out <- cpp_row_id(o, group_sizes, ascending)
  }
  out
}
#' @rdname group_id
#' @export
row_id.GRP <- function(data, ascending = TRUE){
  size <- GRP_data_size(data)
  group_sizes <- GRP_group_sizes(data)
  # If groups are sorted we can use sequence()
  if (GRP_is_sorted(data)){
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequence(group_sizes, from = start, by = every)
  } else {
    group_order <- GRP_order(data)
    out <- cpp_row_id(order = group_order,
                      group_sizes = group_sizes,
                      ascending = ascending)
  }
  out
}
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., ascending = TRUE,
                       .by = NULL, .cols = NULL,
                       .name = NULL){
  UseMethod("add_row_id")
}
#' @rdname group_id
#' @export
add_row_id.data.frame <- function(data, ..., ascending = TRUE,
                                  .by = NULL, .cols = NULL,
                                  .name = NULL){
  if (is.null(.name)){
    .name <- unique_col_name(names(data), "row_id")
  }
  N <- df_nrow(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  # data <- group_info[["data"]]
  extra_groups <- group_info[["extra_groups"]]
  group_vars <- group_info[["dplyr_groups"]]
  groups_changed <- group_info[["groups_changed"]]
  all_groups <- group_info[["all_groups"]]
  if (length(all_groups) == 0L){
    if (ascending){
      row_ids <- seq_len(N)
    } else {
      row_ids <- seq.int(length.out = N, from = N, by = -1L)
    }
  } else {
    if (length(extra_groups) == 0 &&
        length(group_vars) == length(group_vars(data)) &&
        !groups_changed){
      groups <- group_data(data)
      o <- unlist(groups[[".rows"]])
      sizes <- cheapr::lengths_(groups[[".rows"]])
      row_ids <- cpp_row_id(o, sizes, ascending)
    } else {
      row_ids <- row_id(f_select(group_info[["data"]], .cols = all_groups),
                        ascending = ascending)
    }
  }
  col_to_add <- add_names(list(row_ids), .name)
  out <- dplyr::dplyr_col_modify(df_ungroup(data), col_to_add)
  reconstruct(data, out)
}

group_id_to_qg <- function(x,
                           n_groups = NULL,
                           group_starts = NULL,
                           group_sizes = NULL,
                           ordered = FALSE,
                           set = FALSE){
  if (is.null(n_groups)){
    n_groups <- collapse::fnunique(x)
  }
  x <- add_attr(x, "N.groups", n_groups, set = set)
  if (!is.null(group_starts)){
    x <- add_attr(x, "starts", group_starts, set = set)
  }
  if (!is.null(group_sizes)){
    x <- add_attr(x, "group.sizes", group_sizes, set = set)
  }
  if (ordered){
    x <- add_attr(x, "class", c("qG", "ordered", "na.included"), set = set)
  } else {
    x <- add_attr(x, "class", c("qG", "na.included"), set = set)
  }
  x
}
# Efficiently convert qG to integer
qg_to_integer <- function(x, set = FALSE){
  strip_attrs(x, set = set)
}

# Like collapse::qG()
# Except group_id methods can be used
# A little bit less efficient when na_exclude is true
quick_group <- function(x, order = TRUE, ascending = TRUE, na_exclude = FALSE){
  ids <- group_id(x, order = order, as_qg = TRUE, ascending = ascending)
  n_groups <- attr(ids, "N.groups")
  starts <- attr(ids, "starts")
  sizes <- attr(ids, "group.sizes")

  out <- ids

  if (na_exclude){
    if (length(ids) > 0 && cheapr::any_na(x)){
      out <- strip_attrs(out)
      na_loc <- cheapr::which_na(x)
      out[na_loc] <- NA
      if (order){
        attr(out, "n_groups") <- n_groups - 1L
        attr(out, "starts") <- starts[seq_len(length(starts) - 1L)]
        attr(out, "group.sizes") <- sizes[seq_len(length(sizes) - 1L)]
      } else {
        unique_ids <- out[starts]
        unique_non_na_loc <- cheapr::which_not_na(unique_ids)
        first_na_loc <- cheapr::which_na(unique_ids)
        is_na_last_group <- first_na_loc == n_groups
        if (!is_na_last_group){
          id_locs <- which(out > first_na_loc)
          out[id_locs] <- out[id_locs] - 1L
        }
        attr(out, "n_groups") <- n_groups - 1L
        attr(out, "starts") <- starts[unique_non_na_loc]
        attr(out, "group.sizes") <- sizes[unique_non_na_loc]
      }
    }
    class(out) <- setdiff(class(ids), "na.included")
  }
  out
}
