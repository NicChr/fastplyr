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
  df_as_tbl(cheapr::list_as_df(x))
}

# df manipulation helpers -------------------------------------------------

# This is not only faster than dplyr col modify for large data frames
# but also works with data.tables because of reconstruct.data.table
df_rm_cols <- function(data, cols){
  cheapr::df_modify(data, `names<-`(cheapr::new_list(length(cols)), col_select_names(data, cols)))
}

df_add_col <- function(data, col, value){
  cheapr::df_modify(data, `names<-`(list(value), col))
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

grouped_df_counts <- function(data, weights = NULL, expand = FALSE){
  if (!is.null(weights)){
    if (length(weights) != df_nrow(data)){
      cli::cli_abort("Weights must satisfy `length(weights) == nrow(data)`")
    }

    group_id <- f_group_indices(data)
    if (expand){
      counts <- collapse::fsum(as.double(weights), g = group_id,
                               TRA = "replace_fill", na.rm = TRUE)
    } else {
      counts <- collapse::fsum(as.double(weights), g = group_id,
                               use.g.names = FALSE, na.rm = TRUE)
    }
  } else {
    counts <- f_group_size(data)
    if (expand){
      counts <- cheapr::sset(counts, f_group_indices(data))
    }
  }
  counts
}

df_group_by_drop_default <- cpp_group_by_drop_default

cross_join2 <- function(x, y, .repair_names = FALSE){
  cheapr::col_c(
    cheapr::cheapr_rep_each(x, cheapr::vector_length(y)),
    cheapr::cheapr_rep(y, cheapr::vector_length(x)),
    .name_repair = .repair_names, .recycle = FALSE
  )
}

cross_join <- function(...){
  dots <- list_tidy(..., .named = TRUE, .keep_null = FALSE)
  out <- Reduce(cross_join2, unname(dots))
  if (!is_df(out)){
    out <- new_tbl(x = out)
  }
  names(out) <- cheapr::str_coalesce(names(out), names(dots))
  names(out) <- cheapr::name_repair(names(out))
  out
}

## Mutate maybe some variables that aren't atomic or are exotic
## using group_id methods

df_mutate_exotic_to_ids <- function(x, order = TRUE, as_qg = FALSE){
  cpp_df_transform_exotic(x, order = order, as_qg = as_qg)
}

expand_unused_levels <- function(data){
  is_factor <- vapply(data, is.factor, FALSE, USE.NAMES = FALSE)
  if (any(is_factor)){
    non_factors <- cheapr::sset_col(data, !is_factor)
    factors <- cheapr::sset_col(data, is_factor)
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
      missed <- f_anti_join(full, cheapr::sset_col(data, names(full)))
      missed <- cheapr::df_modify(missed, na_init(non_factors, num_missing_categories))
      data <- f_bind_rows(data, missed)
    }
  }
  data
}

datasets_identical <- function(x, y, cols){
  left <- cheapr::sset_col(x, cols)
  right <- cheapr::sset_col(y, cols)

  if (df_ncol(left) != df_ncol(right)){
    FALSE
  } else {
    all(cpp_frame_addresses_equal(left, right)) ||
      identical(left, right)
  }

}
