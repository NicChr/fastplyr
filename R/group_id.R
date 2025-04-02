#' Fast group and row IDs
#'
#' @description
#' These are tidy-based functions for calculating group IDs and row IDs. \cr
#'
#' *  `group_id()` returns an integer vector of group IDs
#' the same size as the `x`.
#' *  `row_id()` returns an integer vector of row IDs.
#' * `f_consecutive_id()` returns an integer vector of consecutive run IDs.
#'
#' The `add_` variants add a column of group IDs/row IDs.
#'
#' @param x A vector or data frame.
#' @param order Should the groups be ordered?
#' When order is `TRUE` (the default) the group IDs will be
#' ordered but not sorted. \cr
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param ascending Should the order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `row_id()` this determines if the row IDs are in
#' increasing or decreasing order. \cr
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#'
#' @details
#'
#' \bold{Note} - When working with data frames it is highly recommended
#' to use the `add_` variants of these functions. Not only are they more
#' intuitive to use, they also have optimisations for large numbers of groups.
#'
#' ### `group_id`
#' This assigns an integer value to unique elements of a vector or unique
#' rows of a data frame. It is an extremely useful function for analysis
#' as you can compress a lot of information into a single column, using that
#' for further operations.
#'
#' ### `row_id`
#' This assigns a row number to each group. To assign plain row numbers
#' to a data frame one can use `add_row_id()`.
#' This function can be used in rolling calculations, finding duplicates and
#' more.
#'
#' ### `consecutive_id`
#'
#' An alternative to `dplyr::consecutive_id()`, `f_consecutive_id()` also
#' creates an integer vector with values in the range `[1, n]` where
#' `n` is the length of the vector or number of rows of the data frame.
#' The ID increments every time `x[i] != x[i - 1]` thus giving information on
#' when there is a change in value.
#' `f_consecutive_id` has a very small overhead in terms
#' of calling the function, making it suitable for repeated calls.
#'
#' @returns
#' An integer vector.
#'
#' @seealso [add_group_id] [add_row_id] [add_consecutive_id]
#'
#' @rdname group_id
#' @export
group_id <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){

  # Use hashing for S4 objects and non-data frame lists

  if (!is_df(x) && cpp_is_exotic(x)){
    return(
      group_id_hash(
        x, order = order, ascending = ascending, as_qg = as_qg
      )
    )
  }

  if (!as_qg && ascending && cpp_is_simple_atomic_vec(x)){
    out <- collapse::qG(x, sort = order, na.exclude = FALSE)
    return(set_rm_attributes(out))
  }

  g <- GRP2(cpp_ungroup(x),
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
group_id.factor <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  out <- unclass(x)
  if (order && ascending && !as_qg){
    out <- strip_attrs(out)
    out[cheapr::which_na(out)] <- length(levels(x)) + 1L
  } else {
    out <- group_id(out, order = order,
                    ascending = ascending, as_qg = as_qg)
  }
  out
}
#' @export
group_id.list <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_id_hash(as.list(x), order = FALSE, as_qg = as_qg)
}
group_id_hash <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){

  # vctrs internal hashing on lists is very good

  out <- as.integer(vctrs::vec_group_id(x))
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
group_id.Interval <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  X <- interval_separate(x)
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
group_id.GRP <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_ids <- GRP_group_id(x)
  if (!order && GRP_is_ordered(x)){
    out <- group_id(group_ids, order = order, as_qg = as_qg)
  } else {
    out <- group_ids
    if (as_qg){
      out <- group_id_to_qg(out,
                            n_groups = GRP_n_groups(x),
                            group_sizes = GRP_group_sizes(x),
                            group_starts = GRP_starts(x),
                            ordered = order)
    }
  }
  out
}
#' @export
group_id.vctrs_rcrd <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_id(
    list_as_df(x),
    order = order,
    ascending = ascending,
    as_qg = as_qg
  )

}
#' @export
group_id.NULL <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  NULL
}
#' @export
group_id.integer64 <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  group_id(
    cpp_int64_to_numeric(x),
    order = order,
    ascending = ascending,
    as_qg = as_qg
  )

}
#' @rdname group_id
#' @export
row_id <- function(x, ascending = TRUE){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(x, ascending = TRUE){
  o <- radixorderv2(x, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is.null(o)){
    return(seq_len(NROW(x)))
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
#' @export
row_id.GRP <- function(x, ascending = TRUE){
  size <- GRP_data_size(x)
  group_sizes <- GRP_group_sizes(x)
  # If groups are sorted we can use sequence()
  if (GRP_is_sorted(x)){
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequence(group_sizes, from = start, by = every)
  } else {
    group_order <- GRP_order(x)
    out <- cpp_row_id(order = group_order,
                      group_sizes = group_sizes,
                      ascending = ascending)
  }
  out
}
#' @rdname group_id
#' @export
f_consecutive_id <- cpp_consecutive_id

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
