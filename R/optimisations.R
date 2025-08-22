#' Enable and disable fastplyr optimisations
#'
#' @name optimisations
#'
#' @description
#' Enable or disable optimisations for common functions package-wide.
#'
#' @returns
#' Enables or disables optimisations invisibly.
#'
#' @seealso [get_group_unaware_fns]
#'
#' @rdname optimisations
#' @export
fastplyr_enable_optimisations <- function(){
  options(fastplyr.optimise = TRUE)
  cli::cli_inform(c("i" = "Function optimisations have been {cli::col_green('enabled')} package-wide"))
}
#' @rdname optimisations
#' @export
fastplyr_disable_optimisations <- function(){
  options(fastplyr.optimise = FALSE)
  cli::cli_inform(c("i" = "Function optimisations have been {cli::col_red('disabled')} package-wide"))
}
