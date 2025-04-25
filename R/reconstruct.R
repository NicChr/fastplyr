
#' @exportS3Method cheapr::reconstruct
reconstruct.grouped_df <- function(x, template, ...){

  plain_tbl <- fast_tbl()

  template_groups <- group_vars(template)

  # If groups in template are all in data AND
  # the data relating to groups in template
  # are identical to those in data, then no need to recalculate

  groups_are_identical <-
    all(template_groups %in% names(x)) &&
    datasets_identical(x, template, template_groups)

  if (groups_are_identical){
    groups <- attr(template, "groups")
  } else {
    out_groups <- vec_intersect(template_groups, names(x))
    if (length(out_groups) == 0L){
      groups <- NULL
    } else {
      drop <- df_group_by_drop_default(template)
      GRP <- df_to_GRP(f_ungroup(x), out_groups, order = TRUE)
      groups <- construct_dplyr_group_data(GRP, drop = drop)
    }
  }
  out <- cheapr::reconstruct(x, cpp_ungroup(template))
  attr(out, "groups") <- groups
  class(out) <- class(template)
  out
}


#' @exportS3Method cheapr::reconstruct
reconstruct.fastplyr_grouped_df <- function(x, template, ...){

  plain_tbl <- fast_tbl()

  template_groups <- group_vars(template)

  # If groups in template are all in data AND
  # the data relating to groups in template
  # are identical to those in data, then no need to recalculate

  groups_are_identical <-
    all(template_groups %in% names(x)) &&
    datasets_identical(x, template, template_groups)

  if (groups_are_identical){
    groups <- attr(template, "groups")
    GRP <- attr(template, "GRP")
  } else {
    out_groups <- vec_intersect(template_groups, names(x))
    if (length(out_groups) == 0L){
      groups <- NULL
    } else {
      drop <- df_group_by_drop_default(template)
      order <- group_by_order_default(template)
      ordered <- attr(attr(template, "groups"), "ordered")
      GRP <- df_to_GRP(f_ungroup(x), out_groups, order = order)
      groups <- construct_fastplyr_group_data(GRP, drop = drop)
    }
  }
  out <- cheapr::reconstruct(x, cpp_ungroup(template))
  attr(out, "groups") <- groups
  attr(out, "GRP") <- GRP
  class(out) <- class(template)
  out
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.fastplyr_grouped_df <- function(data, template){
  cheapr::reconstruct(data, template)
}
