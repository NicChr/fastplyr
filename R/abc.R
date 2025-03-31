#' @noRd

# General 'abc' utilities for fastplyr

check_length <- function(x, size){
  if (length(x) != size){
   cli::cli_abort("{.arg x} must be of length {size}")
  }
}

# Coalesce empty strings instead of NA
str_coalesce <- function(...) cpp_str_coalesce(list(...))

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
    out_names <- str_coalesce(names(.cols), out_names)
  }
  `names<-`(match(out, data_names), out_names)
}

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
    out_names <- str_coalesce(names(.cols), out_names)
  }
  `names<-`(out, out_names)
}

# (Internal) Fast col rename
col_rename <- function(data, .cols = integer()){
  .cols <- .subset(.cols, nzchar(names(.cols)))
  out_nms <- names(.cols)
  if (length(out_nms) == 0L){
    return(data)
  }
  data_nms <- names(data)
  if (is.character(.cols)){
    pos <- add_names(match(.cols, data_nms), out_nms)
  } else {
    pos <- .cols
  }
  pos_nms <- names(pos)
  renamed <- .subset(data_nms, pos) != pos_nms
  names(data)[.subset(pos, renamed)] <- .subset(out_nms, renamed)
  data
}

# N expressions in ...
dots_length <- function(...){
  nargs()
}

# Check if signs are all equal
# Special function to handle -0 selection
# Returns 1 or -1, with special handling of -0 to allow slicing of all rows
slice_sign <- function(x){
  if (length(x) == 0){
    1L
  } else if (length(x) == 1){
    cheapr::int_sign(1/x)
  } else {
    rng <- collapse::frange(x, na.rm = FALSE)
    rng_sum <- sum(cheapr::int_sign(1/rng))
    if (abs(rng_sum) != 2){
      cli::cli_abort("Can't mix negative and positive locations")
    }
    cheapr::int_sign(rng_sum)
  }
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
  if (set){
    set_rm_attributes(x)
  } else {
    attributes(x) <- NULL
    x
  }
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

# setdiff and intersect but no deduplicating
fast_setdiff <- function(x, y){
  x[match(x, y, nomatch = 0L) == 0L]
}
fast_intersect <- function(x, y){
  x[match(x, y, nomatch = 0L) != 0L]
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

# Common function used to find locations of empty strings ''
empty_str_locs <- function(x){
  cheapr::val_find(nzchar(x), TRUE, invert = TRUE)
}
# Get namespace of function
# fun_ns <- function(x, env = rlang::caller_env()){
#   cpp_fun_ns(x, env)
# }

# R version
# fun_ns <- function(x, env = rlang::caller_env()){
#   if (!is.function(x)){
#     x <- get0(x, env)
#     # x <- cpp_get(x, env)
#   }
#   env <- environment(x)
#   if (is.null(x) || is.null(env) || !is.function(x)){
#     ""
#   } else if (isBaseNamespace(env)){
#     "base"
#   } else {
#     .getNamespaceInfo(env, "spec")[["name"]] %||% ""
#   }
# }
