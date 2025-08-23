#' Setting global fastplyr options
#'
#' @name fastplyr_options
#'
#' @description
#' Helper functions to allow users to:
#' - Enable or disable optimisations for common functions package-wide
#' - Enable or disable informative messages
#'
#' @returns
#' Enables or disables fastplyr global options invisibly.
#'
#' @seealso [get_group_unaware_fns]
#'
#' @rdname fastplyr_options
#' @export
fastplyr_enable_optimisations <- function(){
  options(fastplyr.optimise = TRUE)
  cli::cli_inform(c("i" = "Function optimisations have been {cli::col_green('enabled')} package-wide"))
}
#' @rdname fastplyr_options
#' @export
fastplyr_disable_optimisations <- function(){
  options(fastplyr.optimise = FALSE)
  cli::cli_inform(c("i" = "Function optimisations have been {cli::col_red('disabled')} package-wide"))
}
#' @rdname fastplyr_options
#' @export
fastplyr_enable_informative_msgs <- function(){
  options(fastplyr.inform = TRUE)
  cli::cli_inform(c("i" = "Informative messages have been {cli::col_green('enabled')} package-wide"))
}
#' @rdname fastplyr_options
#' @export
fastplyr_disable_informative_msgs <- function(){
  options(fastplyr.inform = FALSE)
  cli::cli_inform(c("i" = "Informative messages have been {cli::col_red('disabled')} package-wide"))
}
