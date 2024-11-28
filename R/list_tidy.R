#' Alternative to `rlang::list2`
#'
#' @description
#' Evaluates arguments dynamically like `rlang::list2` but objects
#' created in `list_tidy` have precedence over environment objects.
#'
#' @param ... Dynamic name-value pairs.
#' @param .keep_null `[logical(1)]` - Should `NULL` elements be kept?
#' Default is `TRUE`.
#' @param .named `[logical(1)]` - Should all list elements be named?
#' Default is `FALSE`.
#'
#' @export
list_tidy <- function(..., .keep_null = TRUE, .named = FALSE){
  quos <- rlang::quos(..., .ignore_empty = "all")
  quo_nms <- names(quos)
  out <- cheapr::new_list(length(quos))

  # quo_nms2 is for assigning objs to our new environment and so
  # they can't be empty strings
  quo_nms2 <- quo_nms

  if (is.null(quo_nms2)){
    quo_nms2 <- quo_labels(quos, named = FALSE)
  } else {
    empty <- empty_str_locs(quo_nms2)
    if (length(empty) > 0){
      quo_nms2[empty] <- quo_labels(quos[empty], named = FALSE)
    }
  }
  if (.named){
    names(out) <- quo_nms2
  } else {
    names(out) <- quo_nms
  }
  new_env <- list2env(list(), parent = emptyenv())
  mask <- rlang::new_data_mask(new_env)
  mask$.data <- rlang::as_data_pronoun(new_env)

  for (i in seq_along(quos)){
    result <- rlang::eval_tidy(quos[[i]], mask)
    new_env[[quo_nms2[[i]]]] <- result
    if (!is.null(result)){
      out[[i]] <- result
    }
  }
  if (!.keep_null){
    out <- list_rm_null(out)
  }
  out
}
