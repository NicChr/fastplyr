#' A convenience function to group by every row
#'
#' @description
#' fastplyr currently cannot handle `rowwise_df` objects created through
#' `dplyr::rowwise()` and so this is a convenience function to allow you to
#' perform row-wise operations.
#' For common efficient row-wise functions,
#' see the 'kit' package.
#'
#' @param data data frame.
#' @param ... Variables to group by using `tidyselect`.
#' @param .ascending Should data be grouped in ascending row-wise order?
#' Default is `TRUE`.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .name Name of row-id column to be added.
#'
#' @returns
#' A row-wise `grouped_df`.
#' @export
f_rowwise <- function(data, ..., .ascending = TRUE,
                      .cols = NULL,
                      .name = ".row_id"){
  rowwise_locs <- tidy_select_pos(data, ..., .cols = .cols)
  rowwise_cols <- names(data)[rowwise_locs]
  out <- add_row_id(data, .cols = rowwise_cols, .name = .name, .ascending = .ascending)

  row_id_nm <- names(out)[length(names(out))]
  groups <- as_tbl(
    f_select(
      df_ungroup(out),
      .cols = c(rowwise_cols, row_id_nm)
    )
  )

  if (!.ascending){
    rev_seq <- seq(from = df_nrow(out), by = -1L, length.out = df_nrow(out))
    groups <- cheapr::sset_df(groups, rev_seq)
    row_ids <- as.list(rev_seq)
  } else {
    row_ids <- as.list(df_seq_along(out))
  }

  row_ids <- vctrs_new_list_of(row_ids, integer())
  groups[[".rows"]] <- row_ids

  attr(groups, "ordered") <- FALSE
  attr(groups, ".drop") <- df_group_by_drop_default(data)
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
