##### Data frame helpers #####


# df checkers -------------------------------------------------------------


is_df <- function(x){
  inherits(x, "data.frame")
}

check_is_df <- function(x){
  if (!is_df(x)){
    cli::cli_abort("{.arg x} must be a {.cls data.frame}")
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
group_data <- cpp_group_data
group_vars <- cpp_group_vars
group_rows <- cpp_group_rows
group_keys <- function(x){
  cheapr::reconstruct(cpp_group_keys(x), cpp_ungroup(x))
}

# df/list constructors ----------------------------------------------------

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

# This is not only faster than dplyr col modify for large data frames
# but also works with data.tables because of reconstruct.data.table
df_rm_cols <- function(data, cols){
  df_add_cols(data, `names<-`(cheapr::new_list(length(cols)), col_select_names(data, cols)))
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
    cli::cli_abort("{.arg times} must not be greater than {nrow(data)}")
  }
  if (length(times) != N){
    if (length(times) != 1L){
      cli::cli_abort("{.arg times} must be of length 1 or {nrow(data)}")
    }
  }
  if (df_ncol(data) == 0){
    if (length(times) == 1){
      out <- data
      attr(out, "row.names") <- .set_row_names(N * times)
    } else {
      out <- cheapr::sset_row(data, rep(attr(data, "row.names"), times))
    }
  } else {
    out <- list_as_df(lapply(data, \(x) rep(x, times)))
  }
  cheapr::reconstruct(out, data)
}
# Repeat each row
df_rep_each <- function(data, each){
  if (length(each) == 1L){
    each <- rep_len(each, df_nrow(data))
  }
  df_rep(data, each)
}

# Safe ungroup for any data type
df_ungroup <- cpp_ungroup
df_is_sorted <- function(data){
  df_order <- radixorderv2(data)
  isTRUE(attr(df_order, "sorted"))
}

df_paste_names <- function(data,  sep = "_", .cols = names(data)){
  do.call(paste, c(f_select(data, .cols = .cols),
                   list(sep = sep)))
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
  names(out) <- cheapr::name_repair(names(out))
  out
}

## Mutate maybe some variables that aren't atomic or are exotic
## using group_id methods

df_mutate_exotic_to_ids <- function(x, order = TRUE, as_qg = FALSE){
  cpp_df_transform_exotic(x, order = order, as_qg = as_qg)
}
