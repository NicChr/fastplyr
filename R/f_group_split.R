#' Alternative to `dplyr::group_split`
#'
#'
#' @inheritParams f_nest_by
#'
#' @returns
#' A list of data frames split by group.
#'
#' @export
f_group_split <- function(.data, ..., .add = FALSE,
                          .order = group_by_order_default(.data),
                          .by = NULL, .cols = NULL,
                          .drop = df_group_by_drop_default(.data)){

  .data %>%
    f_group_by(..., .cols = .cols, .order = .order, .add = .add,
               .by = {{ .by }}, .drop = .drop
    ) %>%
    cpp_group_split()
}
