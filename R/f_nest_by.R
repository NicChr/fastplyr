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
  out <- data %>%
    f_group_by(..., .cols = .cols, .order = .order,
               .by = {{ .by }},
               .drop = .drop) %>%
    group_data()
  group_vars <- names(out[seq_len(length(out) - 1)])
  temp <- f_select(data, .cols = fast_setdiff(names(data), group_vars))
  fast_df_sset <- get_from_package("cpp_sset_df", "cheapr")
  fast_df_sset2 <- function(i, x){
    fast_df_sset(x, i)
  }
  out[["data"]] <- lapply(out[[".rows"]], fast_df_sset2, temp)
  out[["data"]] <- lapply(out[["data"]], df_as_tbl)
  out[[".rows"]] <- NULL
  out[["data"]] <- vctrs_new_list_of(out[["data"]], cheapr::sset(as_tbl(temp), 0))
  groups <- f_select(out, .cols = group_vars)
  attr(groups, ".drop") <- .drop
  attr(groups, "ordered") <- .order
  groups[[".rows"]] <- vctrs_new_list_of(as.list(seq_len(df_nrow(groups))), integer())
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
