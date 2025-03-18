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

  cpp_list_tidy(fastplyr_quos(..., .named = .named), .keep_null)

  # quos <- fastplyr_quos(..., .named = .named)
  # env <- new.env(hash = FALSE, parent = emptyenv())
  # mask <- rlang::new_data_mask(env)
  # mask$.data <- rlang::as_data_pronoun(env)
  #
  # out <- cpp_eval_all_tidy(quos, mask)
  #
  # if (!.keep_null){
  #   out <- list_rm_null(out)
  # }
  # out






  # quos <- fastplyr_quos(..., .named = .named)
  # quo_nms <- names(quos)
  # out <- cheapr::new_list(length(quos))
  #
  # new_env <- new.env(hash = FALSE, parent = emptyenv())
  # mask <- rlang::new_data_mask(new_env)
  # mask$.data <- rlang::as_data_pronoun(new_env)
  #
  # for (i in seq_along(quos)){
  #   result <- rlang::eval_tidy(quos[[i]], mask, NULL)
  #   nm <- quo_nms[[i]]
  #   if (nzchar(nm)){
  #     new_env[[nm]] <- result
  #   }
  #   if (!is.null(result)){
  #     out[[i]] <- result
  #   }
  # }
  # names(out) <- quo_nms
  # if (!.keep_null){
  #   out <- list_rm_null(out)
  # }
  # out
}
