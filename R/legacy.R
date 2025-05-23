#' @noRd

# Legacy functions to not break dependencies ------------------------------

reconstruct <- function(template, data, copy_extra_attributes = TRUE){
  UseMethod("reconstruct")
}
#' @export
reconstruct.default <- function(template, data, copy_extra_attributes = TRUE){
  cheapr::rebuild(data, template)
  # out <- cheapr::rebuild(data, template)
  # if (copy_extra_attributes){
  #   attrs <- attributes(template)
  #   nms <- names(attrs)
  #   nms <- setdiff(nms, c("names", "dim", "dimnames", "row.names", "comment", "class", "tsp", "groups", "GRP"))
  #   if (length(nms) > 0){
  #     # `mostattributes<-`(out, c(attributes(out), attrs[nms]))
  #     attributes(out) <- c(attributes(out), attrs[nms])
  #   }
  # }
  # out
}

df_rep <- cheapr::cheapr_rep
df_rep_each <- cheapr::cheapr_rep_each
df_init <- na_init
df_row_slice <- cheapr::sset

fast_setdiff <- function(x, y){
  x[match(x, y, 0L) == 0L]
}
fast_intersect <- function(x, y){
  x[match(x, y, 0L) != 0L]
}

df_as_one_GRP <- function(data, order = TRUE, return.order = TRUE){
  grouped_df_as_GRP(f_ungroup(data))
}
