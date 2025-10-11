#' Create a subset of data for each group
#'
#' @description
#' A faster `nest_by()`.
#'
#' @inheritParams f_group_by
#'
#' @returns
#' A row-wise `grouped_df` of the corresponding data of each group.
#'
#' @examples
#' library(dplyr)
#' library(fastplyr)
#'
#' # Stratified linear-model example
#'
#' models <- iris |>
#'   f_nest_by(Species) |>
#'   mutate(model = list(lm(Sepal.Length ~ Petal.Width + Petal.Length, data = first(data))),
#'          summary = list(summary(first(model))),
#'          r_sq = first(summary)$r.squared)
#' models
#' models$summary
#'
#' # dplyr's `nest_by()` is admittedly more convenient
#' # as it performs a double bracket subset `[[` on list elements for you
#' # which we have emulated by using `first()`
#'
#' # `f_nest_by()` is faster when many groups are involved
#'
#' models <- iris |>
#'   nest_by(Species) |>
#'   mutate(model = list(lm(Sepal.Length ~ Petal.Width + Petal.Length, data = data)),
#'          summary = list(summary(model)),
#'          r_sq = summary$r.squared)
#' models$summary
#'
#' models$summary[[1]]
#' @export
f_nest_by <- function(.data, ..., .add = FALSE,
                      .order = group_by_order_default(.data),
                      .by = NULL, .cols = NULL,
                      .drop = df_group_by_drop_default(.data)){
  data <- .data |>
    f_group_by(
      ..., .cols = .cols, .order = .order,
      .add = .add, .by = {{ .by }}, .drop = .drop
    )
  GRP <- f_group_GRP(data)
  group_vars <- f_group_vars(data)
  group_keys <- f_group_keys(data)

  data <- as_tbl(data)
  chunks <- vec_group_split(data, GRP)
  chunks <- vctrs::new_list_of(chunks, ptype = cheapr::sset(data, 0))

  out <- cheapr::df_modify(group_keys, list(data = chunks))

  # Below line can be replaced more efficiently by just adding the attributes
  # directly since we know each row is a new group
  # Ultimately it won't make much of a difference as much of the computation
  # is done in splitting the data
  f_group_by(out, .cols = group_vars, .order = .order, .drop = .drop)
}
