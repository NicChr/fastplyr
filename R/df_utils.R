##### Data frame helpers #####

check_is_df <- function(x){
  if (!is_df(x)){
    stop(paste(deparse1(substitute(x)), "must be a data.frame"))
  }
}
# Fast nrow/ncol for data frames
df_nrow <- function(x){
  length(attr(x, "row.names", TRUE))
}
df_ncol <- function(x){
  length(attr(x, "names", TRUE))
}
# Slightly faster dplyr::group_vars
group_vars <- function(x){
  if (is_df(x)){
    if (inherits(x, "grouped_df")){
      out <- setdiff(names(attr(x, "groups")), ".rows")
    } else {
      out <- character()
    }
  } else {
    out <- dplyr::group_vars(x)
  }
  out
}
# Faster group_data() but shows same error msg
group_data <- function(x){
  if (inherits(x, "grouped_df")){
    attr(x, "groups")
  }
  else {
    dplyr::group_data(x)
  }
}
# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL){
  dplyr_groups <- group_vars(data)
  if (rlang::quo_is_null(rlang::enquo(.by))){
    by_groups <- NULL
  } else {
    by_groups <- tidy_select_names(data, {{ .by }})
  }
  if (length(by_groups) > 0L){
    if (length(dplyr_groups) > 0L){
      stop(".by cannot be used on a grouped_df")
    }
    by_groups
  } else {
    dplyr_groups
  }
}


# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  out <- cheapr::sset(data, i)
  if (reconstruct){
    out <- reconstruct(data, out)
  }
  out
}
df_rm_cols <- function(data, .cols){
  cols_to_remove <- col_select_names(data, .cols = .cols)
  dplyr::dplyr_col_modify(data, add_names(vector("list", length(cols_to_remove)),
                                          cols_to_remove))
}
# Seq along df rows/cols
df_seq_along <- function(data, along = "rows"){
  switch(along,
         rows = seq_len(df_nrow(data)),
         seq_len(df_ncol(data)))
}
# Repeat data frame rows
# Such that identical(df_rep(data, 3), bind_rows(data, data, data))
df_rep <- function(data, times){
  N <- df_nrow(data)
  if (N > 0L && length(times) > N){
    stop("times must not be greater than nrow(data)")
  }
  if (length(times) != N){
    if (length(times) != 1L){
      stop("times must be of length 1 or nrow(data)")
    }
  }
  df_row_slice(data, rep.int(df_seq_along(data, "rows"), times = times))
}
# Repeat each row
df_rep_each <- function(data, each){
  if (length(each) == 1L){
    each <- rep_len(each, df_nrow(data))
  }
  df_rep(data, each)
}
# Convenience function
is_df <- function(x){
  inherits(x, "data.frame")
}
df_n_distinct <- function(data){
  GRP_n_groups(
    df_to_GRP(data, .cols = names(data),
              return.groups = FALSE, return.order = FALSE)
  )
}
# list() that removes NULL elements
list3 <- function(...){
  list_rm_null(list(...))
}
# list to tibble/DT
# No checks are done so use with caution
# Cannot contain duplicate names
# or different length list elements
list_as_tbl <- function(x){
  df_as_tbl(list_as_df(x))
}

# Create new df with no name checks or length checks
# ..N is there purely to create an (n > 0) x 0 data frame
new_df <- function(..., ..N = NULL, .recycle = FALSE){
  if (.recycle){
    out <- cheapr::recycle(...)
  } else {
    out <- list3(...)
  }
  if (is.null(..N)){
    if (length(out) == 0L){
      row_names <- integer()
    } else {
      N <- length(.subset2(out, 1L))
      row_names <- c(NA_integer_, -N)
    }
  } else {
    row_names <- .set_row_names(..N)
  }
  attr(out, "names") <- as.character(attr(out, "names", TRUE))
  attr(out, "row.names") <- row_names
  class(out) <- "data.frame"
  out
}
new_tbl <- function(..., ..N = NULL, .recycle = FALSE){
  df_as_tbl(new_df(..., ..N = ..N, .recycle = .recycle))
}
# Safe ungroup for any data type
df_ungroup <- function(data){
  if (inherits(data, "grouped_df")){
    attr(data, "groups") <- NULL
    class(data) <- c("tbl_df", "tbl", "data.frame")
  }
  data
}
df_is_sorted <- function(data){
  df_order <- radixorderv2(data)
  isTRUE(attr(df_order, "sorted"))
}

df_paste_names <- function(data,  sep = "_", .cols = names(data)){
  do.call(paste, c(f_select(data, .cols = .cols),
                   list(sep = sep)))
}

df_as_df <- function(x){
  list_as_df(x)
}
# Faster as_tibble
df_as_tbl <- function(x){
  out <- list_as_df(x)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
# Theoretically safe data frame initialisation
# for all objs with a rep() and [ method
df_init <- function(x, size = 1L){
  ncols <- df_ncol(x)
  if (ncols == 0){
    init_df <- new_df(..N = size)
  } else {
    init_df <- list_as_df(
      lapply(x, function(y) rep(y[NA_integer_], size))
    )
  }
  reconstruct(x, init_df)
}
# Group IDs (same as dplyr::group_indices)
df_group_id <- function(x){
  if (!inherits(x, "grouped_df") && !inherits(x, "data.frame")){
    stop("Can only calculate group indices on data frames")
  }
  N <- df_nrow(x)
  groups <- attr(x, "groups")
  if (is.null(groups)){
    out <- rep_len(1L, N)
  } else {
    out <- cpp_df_group_indices(groups[[".rows"]], N)
  }
  out
}
# Fast/efficient drop empty rows
df_drop_empty <- function(data, .cols = names(data)){
  is_empty_row <- cheapr::row_all_na(cheapr::sset(data, j = .cols))
  which_not_empty <- which(is_empty_row, invert = TRUE)
  if (length(which_not_empty) == df_nrow(data)){
    data
  } else {
    df_row_slice(data, which_not_empty)
  }
}

df_add_cols <- function(data, cols){
  dplyr::dplyr_col_modify(data, cols)
}

# Extremely simple count functions for grouped_df

df_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      stop("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(weights, g = df_group_id(.data), use.g.names = FALSE)
  } else {
    counts <- cheapr::lengths_(groups[[".rows"]])
  }
  out <- f_select(groups, .cols = setdiff(names(groups), ".rows"))
  out[[name]] <- counts
  out
}
df_add_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  group_ids <- df_group_id(.data)
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      stop("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(weights, g = group_ids, TRA = "replace_fill")
  } else {
    counts <- cheapr::lengths_(groups[[".rows"]])[group_ids]
  }
  df_add_cols(.data, add_names(list(counts), name))
}

df_group_by_drop_default <- function(x){
  if (inherits(x, "grouped_df")){
    attr(attr(x, "groups", TRUE), ".drop", TRUE)
  } else {
    TRUE
  }
}
df_group_by_order_default <- function(x){
  if (inherits(x, "grouped_df")){
    out <- attr(attr(x, "groups", TRUE), "sorted", TRUE)
  } else {
    out <- TRUE
  }
  if (is.null(out)){
    TRUE
  } else {
    out
  }
}
# Like dplyr::bind_cols() but written in mostly base R
# and simpler..
df_cbind <- function(..., .repair_names = TRUE, .sep = "..."){
  dots <- list3(...)
  nrow_range <- collapse::.range(cpp_nrows(dots), na.rm = TRUE)
  if (isTRUE(nrow_range[1] != nrow_range[2])){
    stop("All data frames must be of equal size")
  }
  out <- do.call(c, dots)
  if (.repair_names){
    names(out) <- unique_name_repair(names(out))
  }
  out <- list_as_df(out)
  if (length(dots) == 1){
    out <- dots[[1L]]
  } else if (length(dots) > 1){
    N <- nrow_range[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
    template <- dots[[1L]]

    # Special method for grouped_df because
    # we don't need to recalculate groups
    # Since we're not rearranging or renaming variables
    # except in the case of duplicates.

    if (inherits(template, "grouped_df") &&
        all(group_vars(template) %in% names(out))){
      out <- reconstruct(df_ungroup(template), out)
      class(out) <- class(template)
      attr(out, "groups") <- attr(template, "groups")
    } else {
      out <- reconstruct(template, out)
    }
  }
  out
}
unique_name_repair <- function(x, .sep = "..."){
  x <- as.character(x)
  col_seq <- seq_along(x)
  which_dup <- which(collapse::fduplicated(x, all = TRUE))
  x[which_dup] <- paste0(x[which_dup], .sep, col_seq[which_dup])
  x
}

df_cross_join <- function(x, y){
  df_cbind(df_rep_each(x, df_nrow(y)), df_rep(y, df_nrow(x)))
}

cross_join2 <- function(x, y){
  if (!is_df(x)) x <- new_df(x = x)
  if (!is_df(y)) y <- new_df(y = y)
  df_cross_join(x, y)
  # if (!is.list(x)) x <- list(x = x)
  # if (!is.list(y)) y <- list(y = y)
  # df_cross_join(list_as_df(x), list_as_df(y))
}
cross_join <- function(...){
  Reduce(cross_join2, list(...), simplify = FALSE)
}

