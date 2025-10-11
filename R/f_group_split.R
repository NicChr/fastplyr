#' Alternative to `dplyr::group_split`
#'
#'
#' @inheritParams f_nest_by
#' @param .group_names Should group names be added? Default is `FALSE`.
#'
#' @returns
#' A list of data frames split by group.
#'
#' @export
f_group_split <- function(.data, ..., .add = FALSE,
                          .order = group_by_order_default(.data),
                          .by = NULL, .cols = NULL,
                          .drop = df_group_by_drop_default(.data),
                          .group_names = FALSE){

  data <- .data |>
    f_group_by(
      ..., .cols = .cols, .order = .order,
      .add = .add, .by = {{ .by }}, .drop = .drop
    )
  GRP <- f_group_GRP(data)
  out <- vec_group_split(as_tbl(data), GRP)
  if (.group_names){
    names(out) <- GRP_names(GRP)
  }
  out
}
