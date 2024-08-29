#' @description
#'
#' fastplyr is a tidy frontend using a faster and more efficient backend based
#' on two packages, collapse and cheapr.
#'
#' fastplyr includes dplyr and tidyr alternatives that behave like their
#' tidyverse equivalents but are more efficient.
#'
#' Similar in spirit to the excellent tidytable package, fastplyr
#' also offers a tidy frontend that is fast and easy to use. Unlike tidytable,
#' fastplyr verbs are interchangeable with dplyr verbs.
#'
#' You can learn more about the tidyverse, collapse and cheapr
#' using the links below.
#'
#' \href{https://www.tidyverse.org/learn/}{tidyverse}
#'
#' \href{https://sebkrantz.github.io/collapse/articles/collapse_intro.html}{collapse}
#'
#' \href{https://github.com/NicChr/cheapr}{cheapr}
#'
#' @importFrom dplyr .data
#' @importFrom dplyr across
#' @importFrom dplyr all_of
"_PACKAGE"

.datatable.aware <- TRUE
