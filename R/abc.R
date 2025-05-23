#' @noRd

# General 'abc' utilities for fastplyr

check_length <- function(x, size){
  if (length(x) != size){
   cli::cli_abort("{.arg x} must be of length {size}")
  }
}

drop_names <- function(x) `names<-`(x, NULL)

# Fast way of getting named col positions
col_select_pos <- function(data, .cols = character()){
  .cols <- .cols %||% integer()
  data_names <- attr(data, "names", TRUE)
  names(data_names) <- data_names
  out <- data_names[.cols]

  if (anyNA(out)){
    first_na_col <- .cols[cheapr::which_na(out)[1L]]
    if (is.numeric(first_na_col)){
      cli::cli_abort("Location {first_na_col} doesn't exist")
    } else {
      cli::cli_abort("Column {first_na_col} doesn't exist")
    }
  }
  out_names <- names(out)
  if (!is.null(names(.cols))){
    out_names <- cheapr::str_coalesce(names(.cols), out_names)
  }
  `names<-`(match(out, data_names), out_names)
}

# col_select_pos2 <- function(data, .cols = character()){
#   selected_data <- cheapr::sset_col(data, .cols %||% 0L)
#   out <- match(names(selected_data), names(data))
#   names(out) <- cheapr::str_coalesce(names(.cols), names(selected_data))
#   out
# }
# col_select_names <- function(data, .cols = character()){
#   selected_data <- cheapr::sset_col(data, .cols %||% 0L)
#   out <- names(selected_data)
#   names(out) <- cheapr::str_coalesce(names(.cols), out)
#   out
# }

col_select_names <- function(data, .cols = character()){
  .cols <- .cols %||% integer()
  data_names <- attr(data, "names", TRUE)
  names(data_names) <- data_names
  out <- data_names[.cols]

  if (anyNA(out)){
    first_na_col <- .cols[cheapr::which_na(out)[1L]]
    if (is.numeric(first_na_col)){
      cli::cli_abort("Location {first_na_col} doesn't exist")
    } else {
      cli::cli_abort("Column {first_na_col} doesn't exist")
    }
  }
  out_names <- names(out)
  if (!is.null(names(.cols))){
    out_names <- cheapr::str_coalesce(names(.cols), out_names)
  }
  `names<-`(out, out_names)
}

# (Internal) Fast col rename
col_rename <- function(data, .cols = integer()){
  .cols <- col_select_pos(data, .cols)
  names(data)[.cols] <- names(.cols)
  data
}

# N expressions in ...
dots_length <- function(...){
  nargs()
}

# Like deparse1 but has a cutoff in case of massive strings
deparse2 <- function(expr, collapse = " ", width.cutoff = 500L, nlines = 10L, ...){
  paste(deparse(expr, width.cutoff, nlines = nlines, ...), collapse = collapse)
}

add_attr <- function(x, which, value, set = FALSE){
  if (set){
    set_add_attr(x, which, value)
  } else {
    attr(x, which) <- value
    x
  }
}
strip_attrs <- function(x, set = FALSE){
  cheapr::attrs_rm(x, .set = set)
}
add_names <- function(x, value){
  names(x) <- value
  x
}

interval_separate <- function(x){
  start <- attr(x, "start")
  end <- start + strip_attrs(x)
  cheapr::fast_df(start = start, end = end)
}
is_sorted <- function(x){
  isTRUE(!is.unsorted(x))
}

list_subset <- function(x, i, default = NA){
  check_length(default, 1)
  if (length(x) == 0){
    first_element <- NULL
    ptype <- NULL
  } else {
    first_element <- x[[1]]
    ptype <- first_element[0]
  }
  cpp_list_subset(x, ptype, as.integer(i), default)
}

# Very fast unique function
sort_unique <- function(x, sort = FALSE){
  if (sort){
    o <- radixorderv2(x, starts = TRUE, sort = TRUE)
    starts <- NULL
    sorted_starts <- attr(o, "starts")
    sorted <- isTRUE(attr(o, "sorted"))
    subset <- !(length(sorted_starts) == NROW(x) && sorted)
    if (subset){
      starts <- o[sorted_starts]
    }
  } else {
    starts <- attr(group3(x, starts = TRUE), "starts")
    subset <- length(starts) != NROW(x)
  }
  if (subset){
    cheapr::sset(x, starts)
  } else {
    x
  }
}

# rlang infix default NULL value function
`%||%` <- function(x, y) if (is.null(x)) y else x

