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
which <- cheapr::which_
which_not_in <- get_from_package("which_not_in", "cheapr")
which_in <- get_from_package("which_in", "cheapr")
cpp_int64_to_numeric <- get_from_package("cpp_int64_to_numeric", "cheapr")
cpp_loc_set_replace <- get_from_package("cpp_loc_set_replace", "cheapr")
named_list <- get_from_package("named_list", "cheapr")
`%in_%` <- cheapr::`%in_%`

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
    cli::cli_abort("{.arg .cols} must be a {.cls numeric} or {.cls character} vector")
  }
  # is_na <- is.na(out)
  if (cheapr::any_na(out)){
    first_na_col <- .subset(.cols, .subset(cheapr::which_na(out), 1L))
    if (is.numeric(first_na_col)){
      cli::cli_abort("Location {first_na_col} doesn't exist")
    } else {
      cli::cli_abort("Column {first_na_col} doesn't exist")
    }
  }
  out_nms <- names(.cols)
  if (is.null(out_nms)){
    names(out) <- .subset(data_nms, out)
  } else {
    es <- empty_str_locs(out_nms)
    if (length(es)){
      out_nms[es] <- .subset(data_nms, .subset(out, es))
    }
    names(out) <- out_nms
  }
  out
}
col_select_names <- function(data, .cols = NULL){
  pos <- col_select_pos(data, .cols = .cols)
  add_names(names(data)[match(unname(pos), seq_along(data))], names(pos))

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
  cheapr::new_df(start = start, end = end)
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

# Is x a simple lgl, int, dbl, char, cplx or raw?
is_atomic_vec <- function(x){
  !is.object(x) && is.atomic(x) && is.vector(x)
}

# Common function used to find locations of empty strings ''
empty_str_locs <- function(x){
  cheapr::val_find(nzchar(x), TRUE, invert = TRUE)
}
# Get namespace of function
fun_ns <- function(x, env = rlang::caller_env()){
  if (!is.function(x)){
    x <- tryCatch(get(as.character(x), envir = env),
                  error = function(e) ".error")
    if (identical(x, ".error")){
      return("")
    }
  }
  env <- environment(x)
  if (is.null(env)){
    ""
  } else {
    unname(getNamespaceName(env))
  }

}
