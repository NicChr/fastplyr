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
#' models <- iris %>%
#'   f_nest_by(Species) %>%
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
#' models <- iris %>%
#'   nest_by(Species) %>%
#'   mutate(model = list(lm(Sepal.Length ~ Petal.Width + Petal.Length, data = data)),
#'          summary = list(summary(model)),
#'          r_sq = summary$r.squared)
#' models$summary
#'
#' models$summary[[1]]
#' @export
f_nest_by <- function(data, ..., .add = FALSE,
                      .order = df_group_by_order_default(data),
                      .by = NULL, .cols = NULL,
                      .drop = df_group_by_drop_default(data)){
  data %>%
    f_group_by(..., .cols = .cols, .order = .order,
               .by = {{ .by }},
               .drop = .drop) %>%
    cpp_group_split(.drop, .order)
}
