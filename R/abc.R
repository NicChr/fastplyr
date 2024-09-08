#' @noRd

# General 'abc' utilities for fastplyr

get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}

val_rm <- get_from_package("val_rm", "cheapr")
list_as_df <- get_from_package("list_as_df", "cheapr")
list_rm_null <- get_from_package("cpp_list_rm_null", "cheapr")
set_add_attr <- get_from_package("cpp_set_add_attr", "cheapr")
set_add_attributes <- get_from_package("cpp_set_add_attributes", "cheapr")
set_rm_attr <- get_from_package("cpp_set_rm_attr", "cheapr")
set_rm_attributes <- get_from_package("cpp_set_rm_attributes", "cheapr")
df_select <- get_from_package("df_select", "cheapr")
which <- cheapr::which_
which_not_in <- get_from_package("which_not_in", "cheapr")
which_in <- get_from_package("which_in", "cheapr")
which_val <- get_from_package("which_val", "cheapr")

check_length <- function(x, size){
  if (length(x) != size){
    stop(paste(deparse2(substitute(x)), "must be of length", size))
  }
}

# Fast way of getting named col positions
col_select_pos <- function(data, .cols = character()){
  data_nms <- names(data)
  nm_seq <- seq_along(data_nms)
  # Method for when cols is supplied
  if (is.numeric(.cols)){
    rng_sign <- slice_sign(.cols)
    if (rng_sign == -1){
      .cols <- nm_seq[match(nm_seq, abs(.cols), 0L) == 0L]
    } else {
      .cols <- .subset(.cols, .cols != 0)
    }
    out <- match(.cols, nm_seq)
  } else if (is.character(.cols)){
    out <- match(.cols, data_nms)
  } else {
    stop(".cols must be a numeric or character vector")
  }
  # is_na <- is.na(out)
  if (cheapr::any_na(out)){
    first_na_col <- .subset(.cols, .subset(cheapr::which_na(out), 1L))
    if (is.numeric(first_na_col)){
      stop(paste("Location", first_na_col, "doesn't exist",
                 sep = " "))
    } else {
      stop(paste("Column", first_na_col, "doesn't exist",
                 sep = " "))
    }
  }
  out_nms <- names(.cols)
  if (is.null(out_nms)){
    names(out) <- .subset(data_nms, out)
  } else {
    es <- !nzchar(out_nms)
    out_nms[es] <- .subset(data_nms, .subset(out, es))
    names(out) <- out_nms
  }
  out
}
# Tidyselect col names
col_select_names <- function(data, ..., .cols = NULL){
  names(col_select_pos(data, ..., .cols = .cols))
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
  if (length(x)){
    rng <- collapse::frange(x, na.rm = FALSE)
  } else {
    rng <- integer(2L)
  }
  rng_sum <- sum(sign(1 / rng))
  if (abs(rng_sum) != 2){
    stop("Can't mix negative and positive locations")
  }
  as.integer(sign(rng_sum))
}

# Like deparse1 but has a cutoff in case of massive strings
deparse2 <- function(expr, collapse = " ", width.cutoff = 500L, nlines = 5L, ...){
  paste(deparse(expr, width.cutoff, nlines = nlines, ...), collapse = collapse)
}

strip_attrs <- function(x){
  attributes(x) <- NULL
  x
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
  new_df(start = start, end = end)
}
is_sorted <- function(x){
  isTRUE(!is.unsorted(x))
}

# first_obs <- function(x, n = 1L){
#   check_length(n, 1)
#   N <- NROW(x)
#   if (n >= 0) {
#     size <- min(n, N)
#   }
#   else {
#     size <- max(0L, N + n)
#   }
#   cheapr::sset(x, seq_len(size))
# }
# last_obs <- function (x, n = 1L){
#   check_length(n, 1)
#   N <- NROW(x)
#   if (n >= 0) {
#     size <- min(n, N)
#   }
#   else {
#     size <- max(0L, N + n)
#   }
#   cheapr::sset(x, seq.int(from = N - size + 1L, by = 1L, length.out = size))
# }
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

# is_integerable <- function(x){
#   abs(x) <= .Machine$integer.max
# }
all_integerable <- function(x, shift = 0){
  all(
    (abs(collapse::frange(x, na.rm = TRUE)) + shift ) <= .Machine$integer.max,
    na.rm = TRUE
  )
}

# setdiff and intersect but no deduplicating
fast_setdiff <- function(x, y){
  x[match(x, y, nomatch = 0L) == 0L]
}
fast_intersect <- function(x, y){
  x[match(x, y, nomatch = 0L) != 0L]
}

## Turn dot expressions into names
## Used in the below named_dots()

dot_expr_names <- function(...){
  # The below is fast but can sometimes stall with long strings?
  # as.character(substitute(c(...))[-1L])
  vapply(substitute(alist(...))[-1L], deparse2, "", USE.NAMES = FALSE)
}

named_dots <- function(...){
  dots <- list(...)

  dot_nms <- names(dots)

  if (is.null(dot_nms)){
    names(dots) <- dot_expr_names(...)
  } else if (!all(nzchar(dot_nms))){
    empty <- which(!nzchar(dot_nms))
    expr_names <- dot_expr_names(...)
    dot_nms[empty] <- expr_names[empty]
    names(dots) <- dot_nms
  }
  dots
}
na_init <- function(x, size = 1L){
  if (is_df(x)){
    df_init(x, size)
  } else {
    rep(x[NA_integer_], size)
  }
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
