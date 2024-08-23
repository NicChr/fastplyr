reconstruct <- function(template, data, copy_extra_attributes = TRUE){
  UseMethod("reconstruct")
}
#' @export
reconstruct.data.frame <- function(template, data, copy_extra_attributes = TRUE){
  if (!is_df(data)){
    stop("data must be a data.frame")
  }
  ad <- attributes(data)
  at <- attributes(template)
  at[["names"]] <- names(data)
  at[["row.names"]] <- .row_names_info(data, type = 0L)
  if (!copy_extra_attributes){
    at[setdiff(names(at), c("names", "row.names", "class"))] <- NULL
  }
  attributes(data) <- at
  data
}
#' @export
reconstruct.grouped_df <- function(template, data, copy_extra_attributes = TRUE){
  if (!is_df(data)){
    stop("data must be a data.frame")
  }
  ad <- attributes(data)
  at <- attributes(template)
  at[["names"]] <- names(data)
  at[["row.names"]] <- .row_names_info(data, type = 0L)

  template_groups <- group_vars(template)

  # If groups in template are all in data AND
  # the data relating to groups in template
  # are identical to those in data, then no need to recalculate

  groups_are_identical <-
    all(template_groups %in% names(data)) &&
    identical(
      strip_attrs(as.list(cheapr::sset(data, j = template_groups))),
      strip_attrs(as.list(cheapr::sset(template, j = template_groups)))
    )

  if (!groups_are_identical){
    out_groups <- intersect(template_groups, names(data))
    if (length(out_groups) == 0L){
      at[["class"]] <- setdiff(at[["class"]], "grouped_df")
      at[["groups"]] <- NULL
    } else {
      drop_by_default <- df_group_by_drop_default(template)
      order <- df_group_by_order_default(template)
      sorted <- attr(at[["groups"]], "sorted")
      groups <- group_collapse(df_ungroup(data),
                               .cols = out_groups,
                               sort = TRUE,
                               order = order,
                               id = FALSE, start = FALSE,
                               end = FALSE, size = FALSE,
                               loc = TRUE,
                               .drop = drop_by_default)
      groups <- f_rename(groups, .cols = c(".rows" = ".loc"))
      attributes(groups[[".rows"]]) <- attributes(at[["groups"]][[".rows"]])
      for (a in setdiff(names(attributes(groups)),
                         c("row.names", "class", "names"))){
        attr(groups, a) <- NULL
      }
      attr(groups, "sorted") <- sorted
      class(groups) <- c("tbl_df", "tbl", "data.frame")
      attr(groups, ".drop") <- drop_by_default
      at[["groups"]] <- groups
    }
  }
  if (!copy_extra_attributes){
    at[setdiff(names(at), c("names", "row.names", "class", "groups"))] <- NULL
  }
  attributes(data) <- at
  data
}
#' @export
reconstruct.data.table <- function(template, data, copy_extra_attributes = TRUE){
  if (!is_df(data)){
    stop("data must be a data.frame")
  }
  ad <- attributes(data)
  at <- attributes(template)
  row_names <- .row_names_info(data, type = 0L)
  out <- collapse::qDT(data)
  out <- add_attr(out, "row.names", row_names, set = TRUE)
  if (copy_extra_attributes){
    for (a in setdiff(names(at),
                      c("row.names", "names", "class", "sorted", ".internal.selfref"))){
      out <- add_attr(out, a, at[[a]], set = TRUE)
    }
  }
  add_attr(out, "sorted", attr(data, "sorted"), set = TRUE)
}
