##### Data frame helpers #####


# df checkers -------------------------------------------------------------


is_df <- function(x){
  inherits(x, "data.frame")
}

check_is_df <- function(x){
  if (!is_df(x)){
    stop(paste(deparse2(substitute(x)), "must be a data.frame"))
  }
}


# Fast nrow/ncol for data frames
df_nrow <- function(x){
  length(attr(x, "row.names", TRUE))
}
df_ncol <- function(x){
  length(attr(x, "names", TRUE))
}


# Group metadata ----------------------------------------------------------

# Slightly faster dplyr::group_vars
group_vars <- function(x){
  if (is_df(x)){
    if (inherits(x, "grouped_df")){
      out <- fast_setdiff(names(attr(x, "groups")), ".rows")
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

# df/list constructors ----------------------------------------------------

# Turns df into plain df
df_as_df <- function(x){
  list_as_df(x)
}

# Converts df into plain tbl
df_as_tbl <- function(x){
  `class<-`(x, c("tbl_df", "tbl", "data.frame"))
}

# list to tbl
# No checks are done so use with caution
# Cannot contain duplicate names
# or different length list elements
list_as_tbl <- function(x){
  df_as_tbl(list_as_df(x))
}

# df manipulation helpers -------------------------------------------------

# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  out <- cheapr::sset(data, i)
  if (reconstruct){
    out <- reconstruct(data, out)
  }
  out
}
df_add_cols <- function(data, cols){
  reconstruct(data, dplyr::dplyr_col_modify(f_ungroup(data), cols))
}

combine <- function(...){
  if (nargs() == 0) {
    return(NULL)
  }
  dots <- list(...)
  if (length(dots) == 1) {
    dot <- dots[[1L]]
    if (rlang::is_bare_list(dot)){
      return(do.call(combine, dot))
    }
    else {
      return(dot)
    }
  }

  if (cpp_any_frames(dots)){
    do.call(f_bind_rows, dots)
  } else {
    do.call(c, dots)
  }
}

# The below is fine but doesn't work with matrix cols

# df_modify_cols <- function(data, cols){
#   if (!(is.list(cols) && !is.null(names(cols)))){
#     stop("cols must be a named list")
#   }
#   N <- df_nrow(data)
#   out <- unclass(data)
#   temp <- unclass(cols)
#   # temp <- do.call(function(...) cheapr::recycle(..., length = N), cols)
#   for (col in names(temp)){
#     # out[[col]] <- temp[[col]]
#     vec <- temp[[col]]
#     if (NROW(vec) != N){
#       if (is_df(vec)){
#         vec <- cheapr::sset(vec, rep_len(attr(x, "row.names"), N))
#       } else {
#         vec <- rep_len(vec, N)
#       }
#     }
#     out[[col]] <- vec
#   }
#   class(out) <- class(data)
#   reconstruct(data, out)
# }

# df_add_cols2 <- function(data, cols, check = TRUE){
#   if (check){
#     if ( (!(is.list(cols) && !is.object(cols))) || is.null(names(cols))){
#       stop("cols must be a named list")
#     }
#     cols <- do.call(function(...) cheapr::recycle(..., length = df_nrow(data)), cols)
#   }
#   cpp_df_add_cols(data, cols)
# }

# df_rm_cols <- function(data, .cols){
#   cols_to_remove <- col_select_names(data, .cols = .cols)
#   dplyr::dplyr_col_modify(data, add_names(vector("list", length(cols_to_remove)),
#                                           cols_to_remove))
# }

# This is not only faster than dplyr col modify for large data frames
# but also works with data.tables because of reconstruct.data.table
df_rm_cols <- function(data, cols){

  # un-class to ensure no s3 methods are used below
  out <- unclass(data)
  rm_cols <- unname(col_select_pos(data, .cols = cols))

  # Set them to NULL to remove
  out[rm_cols] <- NULL

  class(out) <- class(data)
  reconstruct(data, out)
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

# Safe ungroup for any data type
df_ungroup <- function(data){
  if (inherits(data, "grouped_df")){
    attr(data, "groups") <- NULL
    class(data) <- fast_setdiff(class(data), "grouped_df")
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

# Theoretically safe data frame initialisation
# for all objs with a rep() and [ method
df_init <- function(x, size = 1L){
  ncols <- df_ncol(x)
  if (ncols == 0){
    init_df <- cheapr::new_df(.nrows = size)
  } else {
    init_df <- list_as_df(lapply(x, na_init, size))
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
df_drop_if_all_empty <- function(data){
  drop <- cheapr::row_all_na(data)
  if (length(drop) == 0){
    data
  } else {
    keep <- cheapr::which_(drop, invert = TRUE)
    df_row_slice(data, keep)
  }
}
df_drop_if_any_empty <- function(data){
  drop <- cheapr::row_any_na(data)
  if (length(drop) == 0){
    data
  } else {
    keep <- cheapr::which_(drop, invert = TRUE)
    df_row_slice(data, keep)
  }
}

# Extremely simple count functions for grouped_df

df_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      stop("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(as.double(weights), g = df_group_id(.data),
                             use.g.names = FALSE, na.rm = TRUE)
  } else {
    counts <- cheapr::lengths_(groups[[".rows"]])
  }
  out <- f_select(groups, .cols = fast_setdiff(names(groups), ".rows"))
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
    counts <- collapse::fsum(as.double(weights), g = group_ids,
                             TRA = "replace_fill", na.rm = TRUE)
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
  order_groups <- getOption(".fastplyr.order.groups")
  if (!is.null(order_groups) &&
      !(is.logical(order_groups) &&
        length(order_groups) == 1 &&
        order_groups %in% c(TRUE, FALSE))){
    stop("'.fastplyr.order.groups' option must either `TRUE` or `FALSE`")
  }
  out <- attr(attr(x, "groups", TRUE), "ordered", TRUE)
  if (is.null(out) && inherits(x, "grouped_df")){
    # This implies an implicit ordering through dplyr::group_by()
    TRUE
  } else {
    out %||% (order_groups %||% TRUE)
  }
}
unique_name_repair <- function(x, .sep = "..."){
  if (is.null(x)){
    return(x)
  }
  x <- as.character(x)
  col_seq <- seq_along(x)
  which_dup <- cheapr::which_(collapse::fduplicated(x, all = TRUE))
  x[which_dup] <- paste0(x[which_dup], .sep, col_seq[which_dup])
  which_empty <- empty_str_locs(x)
  x[which_empty] <- paste0(x[which_empty], .sep, col_seq[which_empty])
  x
}

df_cross_join <- function(x, y, .repair_names = TRUE){
  f_bind_cols(
    df_rep_each(x, df_nrow(y)), df_rep(y, df_nrow(x)),
    .repair_names = .repair_names, .recycle = FALSE
  )
}

cross_join2 <- function(x, y){
  if (!is_df(x)) x <- new_tbl(x = x)
  if (!is_df(y)) y <- new_tbl(y = y)
  df_cross_join(x, y, .repair_names = FALSE)
}

cross_join <- function(...){
  dots <- named_list(..., .keep_null = FALSE)
  out <- Reduce(cross_join2, unname(dots))
  if (!is_df(out)){
    out <- new_tbl(x = out)
    names(out) <- names(dots)
  }
  names(out) <- unique_name_repair(names(out))
  out
}

## Mutate maybe some variables that aren't atomic or are exotic
## using group_id methods

df_mutate_exotic_to_ids <- function(x, order = TRUE, ascending = TRUE, as_qg = FALSE){
  check_is_df(x)
  which_exotic <- which(vapply(x, cpp_is_exotic, FALSE, USE.NAMES = FALSE))
  for (i in which_exotic){
    x[[i]] <- group_id(
      x[[i]],
      order = order,
      ascending = ascending,
      as_qg = as_qg
    )
  }
  x
}
