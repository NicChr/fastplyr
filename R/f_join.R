check_suffix <- function(x){
  if (!is.character(x) || length(x) != 2){
    cli::cli_abort("{.arg suffix} must be a {.cls character} vector of length 2")
  }
}

f_join <- function(x, y, by, suffix, multiple, keep, join_type, ...){

  check_suffix(suffix)

  if (inherits(by, "dplyr_join_by")){
    dplyr_by <- unclass(by)

    if (dplyr_by$condition != "=="){
      cli::cli_abort("Currently the only join condition supported is '=='")
    }
    by <- add_names(dplyr_by[["y"]], dplyr_by[["x"]])
  }

  if (is.null(by)){
    by <- vec_intersect(names(x), names(y))
    if (length(by) == 0){
      cli::cli_abort("No common variables, please specify variables to join on")
    }
  }

  if (length(by) == 0 && df_ncol(x) > 0 && df_ncol(y) > 0){
    cli::cli_abort("Please specify variables to join on")
  }

  semi_or_anti <- join_type %in% c("semi", "anti")

  join_cols_right <- unname(by)

  if (is.null(names(by))){
    join_cols_left <- by
  } else {
    join_cols_left <- names(by)
    # Fix empty names
    empty <- cheapr::val_find(nzchar(join_cols_left), FALSE)
    join_cols_left[empty] <- join_cols_right[empty]
  }

  common_cols_left <- vec_intersect(names(x), names(y))
  common_cols_right <- vec_intersect(names(y), names(x))

  unjoinable_cols <- c(vec_setdiff(join_cols_left, names(x)),
                       vec_setdiff(join_cols_right, names(y)))

  if (length(unjoinable_cols) > 0){
    stop(paste0("Unable to join by '", unjoinable_cols[1], "' as it does not exist"))
  }

  non_joined_common_cols_left <- vec_setdiff(common_cols_left, join_cols_left)
  non_joined_common_cols_right <- vec_setdiff(common_cols_right, join_cols_right)

  # Add suffixes

  if (!semi_or_anti){

    names(x)[names(x) %in% non_joined_common_cols_left] <-
      unique_suffix_cols(
        c(names(x), names(y)),
        non_joined_common_cols_left,
        suffix[1L]
      )
    names(y)[names(y) %in% non_joined_common_cols_right] <-
      unique_suffix_cols(
        c(names(x), names(y)),
        non_joined_common_cols_right,
        suffix[2L]
      )

    # If keep join cols from both data frames

    if (keep){
      which_common_join <- which(join_cols_left == join_cols_right)
      common_join_cols_left <- join_cols_left[which_common_join]
      common_join_cols_right <- join_cols_left[which_common_join]

      which_left <- match(common_join_cols_left, names(x))
      common_join_cols_left <- unique_suffix_cols(
        c(names(x), names(y)),
        common_join_cols_left,
        suffix[1L]
      )
      names(x)[which_left] <- common_join_cols_left

      which_right <- match(common_join_cols_right, names(y))
      common_join_cols_right <- unique_suffix_cols(
        c(names(x), names(y)),
        common_join_cols_right,
        suffix[2L]
      )
      names(y)[which_right] <- common_join_cols_right

      join_cols_left[which_common_join] <- common_join_cols_left
      join_cols_right[which_common_join] <- common_join_cols_right
    }
  }

  # Creating a named vector specifying left and right join cols

  join_by <- join_cols_right
  names(join_by) <- join_cols_left

  # Convert vars collapse::join() can't handle into group IDs

  # Need to use all data from x and y to produce accurate group IDs for matching
  # specifically on the joined variables


  exotic_cols_left <- names(x)[vapply(x, cpp_is_exotic, FALSE, USE.NAMES = FALSE)]
  exotic_cols_right <- names(y)[vapply(y, cpp_is_exotic, FALSE, USE.NAMES = FALSE)]

  exotic_join_cols <- join_by[join_cols_right %in% exotic_cols_right | join_cols_left %in% exotic_cols_left]
  exotic_join_cols_left <- names(exotic_join_cols)
  exotic_join_cols_right <- unname(exotic_join_cols)

  exotic_non_join_cols_left <- vec_setdiff(exotic_cols_left, exotic_join_cols_left)
  exotic_non_join_cols_right <- vec_setdiff(exotic_cols_right, exotic_join_cols_right)

  left <- f_ungroup(x)
  right <- f_ungroup(y)

  exotic_data <- cheapr::new_list(length(exotic_join_cols))
  exotic_group_ids <- cheapr::new_list(length(exotic_join_cols))
  names(exotic_data) <- exotic_join_cols_left
  names(exotic_group_ids) <- exotic_join_cols_left

  for (i in seq_along(exotic_join_cols)){
    exotic_combined <- cheapr::cheapr_c(
      left[[exotic_join_cols_left[i]]], right[[exotic_join_cols_right[i]]]
    )
    exotic_data[[i]] <- exotic_combined
    group_ids <- group_id(exotic_combined, order = FALSE)
    exotic_group_ids[[i]] <- group_ids
    left[[exotic_join_cols_left[i]]] <- cheapr::sset(group_ids, df_seq_along(left))
    right[[exotic_join_cols_right[i]]] <- cheapr::sset(group_ids, (df_nrow(left) + 1L):length(group_ids))
  }

  # For the non joining variables, we can just convert directly to IDs

  for (col in exotic_non_join_cols_left){
    left[[col]] <- group_id(left[[col]], order = FALSE)
  }
  for (col in exotic_non_join_cols_right){
    right[[col]] <- group_id(right[[col]], order = FALSE)
  }

  # If any addresses have been changed then yes there are vars
  # in left or right that collapse can't handle
  # We turn these into group IDs and then match them back at the end

  exotic_cols_right <- vec_setdiff(exotic_cols_right, exotic_cols_left)

  # Join

  if (join_type == "anti"){
    out <- cheapr::sset(
      left, which_not_in(
        cheapr::sset_col(left, join_cols_left),
        cheapr::sset_col(right, join_cols_right)
      )
    )
  } else if (join_type == "semi"){
    out <- cheapr::sset(
      left, which_in(
        cheapr::sset_col(left, join_cols_left),
        cheapr::sset_col(right, join_cols_right)
      )
    )
  } else {
    out <- collapse::join(
      left, right, how = join_type,
      on = join_by,
      multiple = multiple,
      drop.dup.cols = FALSE,
      keep.col.order = FALSE,
      verbose = FALSE,
      suffix = "",
      overid = 2L,
      ...
    )

    ## keep all join cols? difficult with collapse::join()

    if (keep){

      inverse_join_by <- names(join_by)
      names(inverse_join_by) <- unname(join_by)
      out <- f_select(out, .cols = c(names(out), inverse_join_by))
      out_nms <- c(names(left), names(right))

      # collapse::join() doesn't keep all join cols
      # So we lose the information on which rows didn't match..

      if (join_type == "left"){
        na_locs <- which_not_in(
          f_select(out, .cols = join_cols_left),
          f_select(right, .cols = add_names(join_cols_right, join_cols_left))
        )
        for (col in join_cols_right){
          out[[col]][na_locs] <- NA
        }
      } else if (join_type == "right"){
        na_locs <- which_not_in(
          cheapr::sset_col(out, join_cols_left),
          cheapr::sset_col(left, join_cols_left)
        )
        for (col in join_cols_left){
          out[[col]][na_locs] <- NA
        }
      } else if (join_type == "full"){
        na_locs_right <- which_not_in(
          f_select(out, .cols = join_cols_left),
          f_select(right, .cols = add_names(join_cols_right, join_cols_left))
        )
        na_locs_left <- which_not_in(
          cheapr::sset_col(out, join_cols_left),
          cheapr::sset_col(left, join_cols_left)
        )
        for (col in join_cols_right){
          out[[col]][na_locs_right] <- NA
        }
        for (col in join_cols_left){
          out[[col]][na_locs_left] <- NA
        }
      } else {
        which_na <- integer()
      }
    } else {
      out_nms <- c(names(left), vec_setdiff(names(right), join_cols_right))
    }
    out <- cheapr::sset_col(out, out_nms)
  }
  # Match group IDs back to original variables

  for (col in exotic_non_join_cols_left){
    matches <- collapse::fmatch(out[[col]], left[[col]], overid = 2L)
    out[[col]] <- cheapr::sset(x[[col]], matches)
  }
  for (col in exotic_join_cols_left){
    matches <- collapse::fmatch(out[[col]], exotic_group_ids[[col]], overid = 2L)
    out[[col]] <- cheapr::sset(exotic_data[[col]], matches)
  }
  for (col in vec_intersect(exotic_join_cols_right, names(out))){
    adjacent_left_col <- exotic_join_cols_left[match(col, exotic_join_cols_right)]
    matches <- collapse::fmatch(out[[col]], exotic_group_ids[[adjacent_left_col]], overid = 2L)
    out[[col]] <- cheapr::sset(exotic_data[[adjacent_left_col]], matches)
  }
  if (!semi_or_anti){
    for (col in vec_intersect(exotic_non_join_cols_right, names(out))){
      matches <- collapse::fmatch(out[[col]], right[[col]], overid = 2L)
      out[[col]] <- cheapr::sset(y[[col]], matches)
    }
  }
  cheapr::rebuild(out, x)
}

#' Fast SQL joins
#'
#' @description
#' Mostly a wrapper around `collapse::join()` that behaves more like
#' dplyr's joins. List columns, lubridate intervals and vctrs rcrds
#' work here too.
#'
#'
#' @param x Left data frame.
#' @param y Right data frame.
#' @param by `character(1)` - Columns to join on.
#' @param suffix `character(2)` - Suffix to paste onto common cols
#' between `x` and `y` in the joined output.
#' @param multiple `logical(1)` - Should multiple matches be returned?
#' If `FALSE` the first match in y is used. Default is `TRUE`.
#' @param keep `logical(1)` - Should join columns from
#' both data frames be kept? Default is `FALSE`.
#' @param ... Additional arguments passed to `collapse::join()`.
#'
#' @returns
#' A joined data frame, joined on the columns specified with `by`, using an
#' equality join.
#'
#' `f_cross_join()` returns all possible combinations
#' between the two data frames.
#'
#' @rdname join
#' @export
f_left_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"),
                        multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "left", ...)
}
#' @rdname join
#' @export
f_right_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"),
                        multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "right", ...)
}
#' @rdname join
#' @export
f_inner_join <- function(x, y, by = NULL,
                         suffix = c(".x", ".y"),
                         multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "inner", ...)
}
#' @rdname join
#' @export
f_full_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"),
                        multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "full", ...)
}
#' @rdname join
#' @export
f_anti_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"),
                        multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "anti", ...)
}
#' @rdname join
#' @export
f_semi_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"),
                        multiple = TRUE, keep = FALSE, ...){
  f_join(x, y, by = by, suffix = suffix,
         multiple = multiple, keep = keep,
         join_type = "semi", ...)
}
#' @rdname join
#' @export
f_cross_join <- function(x, y, suffix = c(".x", ".y"), ...){
  rlang::check_dots_empty0(...)
  check_suffix(suffix)
  names(x) <- unique_suffix_cols(names(x), names(x), suffix[1L])
  names(y) <- unique_suffix_cols(names(y), names(y), suffix[2L])
  cross_join2(x, y, .repair_names = FALSE)
}
#' @rdname join
#' @export
f_union_all <- function(x, y, ...){
  f_bind_rows(x, y, ...)
}
#' @rdname join
#' @export
f_union <- function(x, y, ...){
  sort_unique(f_union_all(x, y))
}

# Iteratively creating unique col names for `f_join()`
unique_suffix_cols <- function(names, cols, suffix){
  if (!nzchar(suffix)){
    return(cols)
  }
  out <- character(length(cols))
  out_names <- character(length(names) + length(cols))
  out_names[seq_along(names)] <- names
  get_unique_col <- function(names, col, suffix){
    col <- stringr::str_c(col, suffix)
    while (col %in% names){
      col <- stringr::str_c(col, suffix)
    }
    col
  }
  for (i in seq_along(cols)){
    out[i] <- get_unique_col(out_names, cols[i], suffix)
    out_names[length(names) + i] <- out[i]

  }
  out
}
