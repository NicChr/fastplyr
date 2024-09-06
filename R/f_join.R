f_join <- function(x, y, by, suffix, multiple, keep, join_type, ...){
  if (!is.character(suffix) || length(suffix) != 2){
    stop("suffix must be a character vector of length 2")
  }

  if (inherits(by, "dplyr_join_by")){
    dplyr_by <- unclass(by)

    if (dplyr_by$condition != "=="){
      stop("Currently the only join condition supported is '=='")
    }
    by <- add_names(dplyr_by[["y"]], dplyr_by[["x"]])
  }

  if (is.null(by)){
    by <- fast_intersect(names(x), names(y))
    if (length(by) == 0){
      stop("No common variables, please specify variables to join on")
    }
  }

  if (length(by) == 0 && df_ncol(x) > 0 && df_ncol(y) > 0){
    stop("Please specify variables to join on")
  }

  semi_or_anti <- join_type %in% c("semi", "anti")

  if (is.null(names(by))){
    join_cols_left <- by
  } else {
    join_cols_left <- names(by)
  }
  join_cols_right <- unname(by)

  common_cols <- fast_intersect(names(x), names(y))

  unjoinable_cols <- c(fast_setdiff(join_cols_left, names(x)),
                       fast_setdiff(join_cols_right, names(y)))

  if (length(unjoinable_cols) > 0){
    stop(paste0("Unable to join by '", unjoinable_cols[1], "' as it does not exist"))
  }

  non_joined_common_cols_left <- fast_setdiff(common_cols, join_cols_left)
  non_joined_common_cols_right <- fast_setdiff(common_cols, join_cols_right)

  # Add suffixes

  if (!semi_or_anti){

    names(x)[names(x) %in% non_joined_common_cols_left] <-
      paste0(names(x)[names(x) %in% non_joined_common_cols_left], suffix[1L])
    names(y)[names(y) %in% non_joined_common_cols_right] <-
      paste0(names(y)[names(y) %in% non_joined_common_cols_right], suffix[2L])

    # If keep join cols from both data frames

    if (keep){
      names(x)[names(x) %in% join_cols_left] <-
        paste0(names(x)[names(x) %in% join_cols_left], suffix[1L])
      names(y)[names(y) %in% join_cols_right] <-
        paste0(names(y)[names(y) %in% join_cols_right], suffix[2L])

      join_cols_left <- paste0(join_cols_left, suffix[1L])
      join_cols_right <- paste0(join_cols_right, suffix[2L])
    }
  }

  # Creating a named vector specifying left and right join cols

  join_by <- join_cols_right
  names(join_by) <- join_cols_left

  # Convert vars collapse::join() can't handle into group IDs

  left <- df_mutate_exotic_to_ids(x)
  right <- df_mutate_exotic_to_ids(y)

  # If any addresses have been changed then yes there are vars
  # in left or right that collapse can't handle
  # We turn these into group IDs and then match them back at the end

  exotic_cols_left <- names(x)[!cpp_address_equal(x, left)]
  exotic_cols_right <- names(y)[!cpp_address_equal(y, right)]
  exotic_cols_right <- fast_setdiff(exotic_cols_right, exotic_cols_left)

  # Join

  if (join_type == "anti"){
    out <- cheapr::sset(
      left, which_not_in(
        f_select(left, .cols = join_cols_left),
        f_select(right, .cols = join_cols_right))
    )
  } else if (join_type == "semi"){
    out <- cheapr::sset(
      left, which_in(
        f_select(left, .cols = join_cols_left),
        f_select(right, .cols = join_cols_right))
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
    if (keep){
      inverse_join_by <- names(join_by)
      names(inverse_join_by) <- unname(join_by)
      out <- f_select(out, .cols = c(names(out), inverse_join_by))
    }
  }

  # Names ordered correctly

  out_nms <- c(names(left), fast_intersect(fast_setdiff(names(right), names(left)), names(out)))
  out <- f_select(out, .cols = out_nms)


  # Match group IDs back to original variables

  if (length(exotic_cols_left) > 0){
    for (col in exotic_cols_left){
      matches <- collapse::fmatch(out[[col]], left[[col]], overid = 2L)
      out[[col]] <- cheapr::sset(x[[col]], matches)
    }
  }
  if (!semi_or_anti){
    if (length(exotic_cols_right) > 0){
      for (col in exotic_cols_right){
        matches <- collapse::fmatch(out[[col]], right[[col]], overid = 2L)
        out[[col]] <- cheapr::sset(y[[col]], matches)
      }
    }
  }
  out
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
#' A joined data frame on the columns specified with `by`, using an
#' equality join.
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
f_union_all <- function(x, y, ...){
  if (is_df(x)){
    f_bind_rows(x, y)
  } else {
    c(x, y)
  }
}
#' @rdname join
#' @export
f_union <- function(x, y, ...){
  sort_unique(f_union_all(x, y))
}

