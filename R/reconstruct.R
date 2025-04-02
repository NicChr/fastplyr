
#' @exportS3Method cheapr::reconstruct
reconstruct.grouped_df <- function(x, template){

  plain_tbl <- fast_tbl()

  template_groups <- group_vars(template)

  # If groups in template are all in data AND
  # the data relating to groups in template
  # are identical to those in data, then no need to recalculate

  groups_are_identical <-
    all(template_groups %in% names(x)) &&
    (
      all(cpp_frame_addresses_equal(
        cheapr::sset_col(x, j = template_groups),
        cheapr::sset_col(template, j = template_groups)
      )) ||
        identical(
          cheapr::sset_col(x, j = template_groups),
          cheapr::sset_col(template, j = template_groups)
        )
    )

  if (groups_are_identical){
    groups <- attr(template, "groups")
  } else {
    out_groups <- fast_intersect(template_groups, names(x))
    if (length(out_groups) == 0L){
      groups <- NULL
    } else {
      drop_by_default <- df_group_by_drop_default(template)
      order <- df_group_by_order_default(template)
      ordered <- attr(attr(template, "groups"), "ordered")
      groups <- group_collapse(df_ungroup(x),
                               .cols = out_groups,
                               sort = TRUE,
                               order = order,
                               id = FALSE, start = FALSE,
                               end = FALSE, size = FALSE,
                               loc = TRUE,
                               .drop = drop_by_default)
      groups <- f_rename(groups, .cols = c(".rows" = ".loc"))
      attributes(groups[[".rows"]]) <- attributes(attr(template, "groups")[[".rows"]])
      groups <- cheapr::reconstruct(groups, plain_tbl)
      attr(groups, "ordered") <- ordered
      attr(groups, ".drop") <- drop_by_default
    }
  }
  out <- cheapr::reconstruct(x, df_ungroup(template))
  attr(out, "groups") <- groups
  class(out) <- class(template)
  out
}
