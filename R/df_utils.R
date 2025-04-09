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
group_keys <- cpp_group_keys

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

df_add_col <- function(data, col, value){
  df_add_cols(data, `names<-`(list(value), col))
}

# Seq along df rows/cols
df_seq_along <- function(data, along = "rows"){
  switch(along,
         rows = seq_len(df_nrow(data)),
         seq_len(df_ncol(data)))
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
df_group_id <- cpp_group_id

# Extremely simple count functions for grouped_df
df_count <- function(.data, name = "n", weights = NULL){
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      cli::cli_abort("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(as.double(weights), g = df_group_id(.data),
                             use.g.names = FALSE, na.rm = TRUE)
  } else {
    counts <- cheapr::list_lengths(group_rows(.data))
  }
  out <- group_keys(.data)
  df_add_cols(out, list_tidy(!!name := counts))
}
df_add_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  group_ids <- df_group_id(.data)
  group_rows <- group_rows(.data)

  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      cli::cli_abort("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(as.double(weights), g = group_ids,
                             TRA = "replace_fill", na.rm = TRUE)
  } else {
    counts <- cheapr::list_lengths(group_rows)[group_ids]
  }
  df_add_cols(.data, list_tidy(!!name := counts))
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
    cheapr::cheapr_rep_each(x, df_nrow(y)), cheapr::cheapr_rep(y, df_nrow(x)),
    .repair_names = .repair_names, .recycle = FALSE
  )
}

cross_join2 <- function(x, y){
  if (!is_df(x)) x <- new_tbl(x = x)
  if (!is_df(y)) y <- new_tbl(y = y)
  df_cross_join(x, y, .repair_names = FALSE)
}

cross_join <- function(...){
  dots <- list_tidy(..., .named = TRUE, .keep_null = FALSE)
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
