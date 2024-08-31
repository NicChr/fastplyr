f_join <- function(x, y, by, suffix, join_type, ...){
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
    by <- intersect(names(x), names(y))
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

  common_cols <- intersect(names(x), names(y))

  unjoinable_cols <- c(setdiff(join_cols_left, names(x)),
                       setdiff(join_cols_right, names(y)))

  if (length(unjoinable_cols) > 0){
    stop(paste0("Unable to join by '", unjoinable_cols[1], "' as it does not exist"))
  }

  non_joined_common_cols_left <- setdiff(common_cols, join_cols_left)
  non_joined_common_cols_right <- setdiff(common_cols, join_cols_right)

  # Add suffixes

  if (!semi_or_anti){

    names(x)[names(x) %in% non_joined_common_cols_left] <-
      paste0(names(x)[names(x) %in% non_joined_common_cols_left], suffix[1L])
    names(y)[names(y) %in% non_joined_common_cols_right] <-
      paste0(names(y)[names(y) %in% non_joined_common_cols_right], suffix[2L])

  }

  # Convert vars collapse::join() can't handle into group IDs

  left <- df_mutate_exotic_to_ids(x)
  right <- df_mutate_exotic_to_ids(y)

  # If any addresses have been changed then yes there are vars
  # in left or right that collapse can't handle
  # We turn these into group IDs and then match them back at the end

  left_exotic_cols <- names(x)[!cpp_address_equal(x, left)]
  right_exotic_cols <- names(y)[!cpp_address_equal(y, right)]
  right_exotic_cols <- setdiff(right_exotic_cols, left_exotic_cols)

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
      on = by,
      multiple = TRUE,
      drop.dup.cols = FALSE,
      keep.col.order = FALSE,
      verbose = FALSE,
      suffix = "",
      overid = 2L,
      ...
    )
  }

  # Names ordered correctly

  out_nms <- c(names(left), intersect(setdiff(names(right), names(left)), names(out)))
  out <- f_select(out, .cols = out_nms)


  # Match group IDs back to original variables

  if (length(left_exotic_cols) > 0){
    for (col in left_exotic_cols){
      matches <- collapse::fmatch(out[[col]], left[[col]], overid = 2L)
      out[[col]] <- cheapr::sset(x[[col]], matches)
    }
  }
if (!semi_or_anti){
  if (length(right_exotic_cols) > 0){
    for (col in right_exotic_cols){
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
#' @param by Columns to join on.
#' @param suffix Suffix to paste onto common cols
#' between `x` and `y` in the joined output.
#' @param ... Additional arguments passed to `collapse::join()`.
#'
#' @rdname join
#' @export
f_left_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "left", ...)
}
#' @rdname join
#' @export
f_right_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "right", ...)
}
#' @rdname join
#' @export
f_inner_join <- function(x, y, by = NULL,
                         suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "inner", ...)
}
#' @rdname join
#' @export
f_full_join <- function(x, y, by = NULL,
                         suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "full", ...)
}
#' @rdname join
#' @export
f_anti_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "anti", ...)
}
#' @rdname join
#' @export
f_semi_join <- function(x, y, by = NULL,
                        suffix = c(".x", ".y"), ...){
  f_join(x, y, by = by, suffix = suffix, join_type = "semi", ...)
}

