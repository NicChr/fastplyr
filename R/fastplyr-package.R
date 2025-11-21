#' @description
#'
#' fastplyr is a tidy front-end using a faster and more efficient back-end based
#' on two packages, collapse and cheapr.
#'
#' fastplyr includes dplyr and tidyr alternatives that behave like their
#' tidyverse equivalents but are more efficient.
#'
#' Similar in spirit to the excellent tidytable package, fastplyr
#' also offers a tidy front-end that is fast and easy to use. Unlike tidytable,
#' fastplyr verbs are interchangeable with dplyr verbs.
#'
#' You can learn more about the tidyverse, collapse and cheapr
#' using the links below.
#'
#' \href{https://tidyverse.org/learn/}{tidyverse}
#'
#' \href{https://sebkrantz.github.io/collapse/articles/collapse_intro.html}{collapse}
#'
#' \href{https://github.com/NicChr/cheapr}{cheapr}
#'
#' @keywords internal
#' @importFrom dplyr .data
#' @importFrom dplyr across
#' @importFrom dplyr pick
#' @importFrom dplyr n
#' @importFrom tidyselect all_of
#' @importFrom tidyselect any_of
#' @importFrom tidyselect contains
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect everything
#' @importFrom tidyselect last_col
#' @importFrom tidyselect num_range
#' @importFrom tidyselect where
#' @importFrom cheapr vector_length
"_PACKAGE"

.datatable.aware <- TRUE
utils::globalVariables(":=")

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @useDynLib fastplyr, .registration = TRUE
## usethis namespace: end
NULL
