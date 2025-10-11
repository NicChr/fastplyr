
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (inherits(data, "grouped_df") && !rlang::quo_is_null(rlang::enquo(.by))){
    cli::cli_abort("{.arg .by} cannot be used on a {.cls grouped_df}")
  }
}
check_cols <- function(n_dots, .cols = NULL){
  if (n_dots > 0 && !is.null(.cols)){
    cli::cli_abort("Cannot supply variables through {.arg ...} and {.arg .cols}, use one argument")
  }
}

# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL, named = FALSE){
  check_rowwise(data)
  dplyr_groups <- f_group_vars(data)
  if (named){
    names(dplyr_groups) <- dplyr_groups
  }
  if (rlang::quo_is_null(rlang::enquo(.by))){
    by_groups <- character()
  } else {
    by_groups <- tidy_select_names(data, {{ .by }})
    if (any(names(by_groups) != by_groups)){
      cli::cli_abort("Can't rename groups through {.arg .by}")
    }
    if (!named){
      attr(by_groups, "names") <- NULL
    }
  }
  if (length(by_groups) > 0L){
    if (length(dplyr_groups) > 0L){
      cli::cli_abort("{.arg .by} cannot be used on a grouped_df")
    }
    by_groups
  } else {
    dplyr_groups
  }
}

# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    dots <- rlang::quos(...)
    select_cols <- character(length(dots))
    for (i in seq_along(dots)){
      expr <- rlang::quo_get_expr(dots[[i]])
      if (is.symbol(expr)){
        select_cols[i] <- rlang::as_string(expr)
      }
    }
    out <- match(select_cols, data_nms)
    if (!anyNA(out)){
     names(out) <- names(dots)
     empty <- cheapr::val_find(nzchar(names(out)), FALSE)
     names(out)[empty] <- data_nms[out[empty]]
      is_dup <- collapse::fduplicated(list(names(out), unname(out)))
      out <- out[!is_dup]
      if (anyDuplicated(names(out))){
        # Use tidyselect for error
        tidyselect::eval_select(rlang::expr(c(...)), data = data)
      }
    } else {
      out <- tidyselect::eval_select(rlang::expr(c(...)), data = data)
    }
  }
  out
}
# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  add_names(names(data)[match(pos, seq_along(data))], names(pos))
}

# workhorse of f_mutate() with metadata
mutate_summary <- function(.data, ...,
                           .keep = "all",
                           .by = NULL,
                           .order = group_by_order_default(.data),
                           .drop = df_group_by_drop_default(.data)){
  original_cols <- names(.data)
  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0){
    GRP <- NULL
    group_keys <- f_group_keys(.data)
    n_groups <- 1L
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
    group_keys <- GRP_groups(GRP)
    n_groups <- GRP_n_groups(GRP)
  }
  quos <- fastplyr_quos(..., .data = .data, .drop_null = FALSE,
                        .unpack_default = FALSE,
                        .optimise = should_optimise(GRP),
                        .optimise_expand = TRUE,
                        .groups = GRP)
  if (dots_length(...) == 0L){
    out_data <- .data
    new_cols <- character()
    used_cols <- character()
    unused_cols <- original_cols
    changed_cols <- character()
  } else {
    new_data <- eval_mutate(.data, quos)
    # Removing duplicate named results
    new_data <- new_data[!duplicated(names(quos), fromLast = TRUE)]
    out_data <- cheapr::df_modify(.data, new_data)
    new_data <- cheapr::list_drop_null(new_data)
    new_cols <- names(new_data)
    all_cols <- names(out_data)
    common_cols <- vec_intersect(original_cols, new_cols)
    changed <- cpp_frame_addresses_equal(
      cheapr::sset_col(.data, common_cols),
      new_data[common_cols]
    )
    changed_cols <- common_cols[cheapr::val_find(changed, FALSE)]
    used_cols <- quo_vars(quos, rlang::as_data_mask(.data), combine = TRUE)
    used_cols <- c(used_cols, vec_setdiff(new_cols, used_cols))
    unused_cols <- vec_setdiff(original_cols, new_cols)

    keep_cols <- switch(.keep,
                        all = all_cols,
                        none = new_cols,
                        used = used_cols,
                        unused = unused_cols)

    # Add missed group vars and keep original ordering
    keep_cols <- vec_intersect(all_cols, c(all_groups, keep_cols))
    out_data <- cheapr::sset_df(out_data, j = keep_cols)
  }
  list(
    data = out_data,
    new_cols = new_cols,
    used_cols = used_cols,
    unused_cols = unused_cols,
    changed_cols = changed_cols,
    all_groups = all_groups,
    GRP = GRP
  )
}

# To replace tidy_group_info_tidyselect in the future
select_summary <- function(.data, ..., .by = NULL, .cols = NULL, .order = group_by_order_default(.data)){

  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0){
    GRP <- NULL
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
  }

  selected_cols <- tidy_select_names(.data, ..., .cols = .cols)
  out <- col_rename(f_ungroup(.data), .cols = selected_cols)
  if (anyDuplicated(names(out))){
    cli::cli_abort("Can't rename columns to names that already exist")
  }
  # Recalculate group vars in case they were renamed
  group_pos <- match(all_groups, names(.data))
  new_group_vars <- names(out)[group_pos]

  if (!identical(new_group_vars, all_groups)){
    cli::cli_abort("Can't rename group vars in this context")
  }
  new_cols <- names(selected_cols)
  used_cols <- new_cols
  unused_cols <- vec_setdiff(names(out), used_cols)
  changed_cols <- character()
  renamed_cols <- used_cols[used_cols != selected_cols]

  list(
    data = cheapr::rebuild(out, .data),
    new_cols = new_cols,
    used_cols = used_cols, # All selected cols
    unused_cols = unused_cols, # Unselected cols
    changed_cols = changed_cols,
    renamed_cols = renamed_cols,
    all_groups = all_groups,
    GRP = GRP
  )
}

tidy_dots_info <- function(.data, ..., .by = NULL, .cols = NULL,
                           .order = group_by_order_default(.data)){
  check_cols(n_dots = dots_length(...), .cols = .cols)
  if (is.null(.cols)){
    mutate_summary(.data, ..., .by = {{ .by }}, .order = .order)
  } else {
    select_summary(.data, ..., .by = {{ .by }}, .order = .order, .cols = .cols)
  }
}

# tidy_GRP applies expressions supplied through `...` or selects cols
# if either .cols is supplied or type isnt "data-mask"
# After that it calculates the grouping structure of these variables

tidy_eval_groups <- function(.data, ..., .by = NULL, .cols = NULL,
                             .order = group_by_order_default(.data),
                             return_order = FALSE){

  info <- tidy_dots_info(.data, ..., .by = {{ .by }}, .cols = .cols,
                         .order = .order)
  data <- info[["data"]]
  groups <- info[["all_groups"]]
  new_cols <- info[["new_cols"]]
  all_groups <- c(vec_setdiff(groups, new_cols), new_cols)
  GRP <- info[["GRP"]]
  # if (length(all_groups) == 0){
  #   GRP <- NULL
  # } else
  if (is.null(GRP) || !identical(groups, all_groups) ||
      length(vec_intersect(info[["changed_cols"]], all_groups)) > 0L){
    GRP <- df_to_GRP(data, all_groups, order = .order, return.order = return_order)
  }
  list(data = data, GRP = GRP)
}

unique_count_col <- function(data, col = "n"){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  if (col %in% data_nms){
    unique_count_col(data, col = paste0(col, "n"))
  } else {
    col
  }
}
# Checks if dataset has a variable name and returns unique name
unique_col_name <- function(data, col){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  i <- 1L
  grp_nm <- col
  while (col %in% data_nms){
    i <- i + 1L
    col <- paste0(grp_nm, i)
  }
  col
}
vctrs_new_list_of <- function(x = list(), ptype){
  structure(x,
            ptype = ptype,
            class = c("vctrs_list_of",
                      "vctrs_vctr",
                      "list"))
}

check_rowwise <- function(data){
  if (inherits(data, "rowwise_df")){
    cli::cli_abort("fastplyr cannot handle {.arg rowwise_df}, please use {.fn f_rowwise}")
  }
}

unpack_results <- function(x, quos){

  quo_names <- names(quos)

  k <- 1L
  for (i in seq_along(x)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(x[[k]])){
      results_to_append <- as.list(x[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      x <- append(x, results_to_append, after = k - 1L)
      k <- k + length(results_to_append)
      x[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }
  x
}

eval_all_tidy_optimised_quos <- function(data, quos){

  quo_names <- names(quos)
  quo_groups <- attr(quos, ".GRP", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)
  group_unaware <- attr(quos, ".group_unaware", TRUE)
  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }

  n_quos <- length(quos)

  mask <- rlang::as_data_mask(data)
  results <- lapply(quos, rlang::eval_tidy, mask)

  group_order <- NULL
  reorder <- FALSE

  if (any(group_unaware)){

    # This is for expressions like `+`
    # where we re-order it to match the order of sorted groups
    # which is the way `reframe()` returns its output

    if (!is.null(GRP) && GRP_n_groups(GRP) > 1){
      group_order <- GRP_order(GRP)
      reorder <- !isTRUE(attr(group_order, "sorted"))
    }

    if (reorder){
      results[group_unaware] <- lapply(
        results[group_unaware], \(x) cheapr::sset(x, group_order)
      )
    }
  }

  groups <- cheapr::new_list(n_quos)

  if (any(group_unaware)){
    reordered_groups <- cheapr::sset_col(data, GRP_group_vars(GRP))
    if (reorder){
      reordered_groups <- cheapr::sset(reordered_groups, group_order)
    }

    reordered_groups <- cheapr::cheapr_rep_len(
      list(reordered_groups), cheapr::val_count(group_unaware, TRUE)
    )

  } else {
    reordered_groups <- list()
  }

  if (any(!group_unaware)){
    reframed_groups <- cheapr::cheapr_rep_len(
      list(
        cheapr::cheapr_rep_len(
          GRP_groups(GRP), vector_length(results[cheapr::which_(group_unaware, invert = TRUE)[1]][[1]])
        )
      ),
      cheapr::val_count(group_unaware, FALSE)
    )
  } else {
    reframed_groups <- list()
  }

  groups[group_unaware] <- reordered_groups
  groups[!group_unaware] <- reframed_groups
  names(groups) <- quo_names
  list(groups = groups, results = results)
}

eval_summarise_optimised_quos <- function(data, quos){

  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }

  n_groups <- GRP_n_groups(attr(quos, ".GRP")) %||% 1

  mask <- rlang::as_data_mask(data)

  out <- lapply(quos, rlang::eval_tidy, mask)
  nrows <- df_nrow(data)

  for (res in out){
    size <- vector_length(res)
    if ( !(size == 0 && nrows == 0 || size == n_groups) ){
     cli::cli_abort("All optimised expressions should return 1 row per-group")
    }
  }

  out
}

eval_mutate_optimised_quos <- function(data, quos){

  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }
  cpp_eval_all_tidy(quos, rlang::as_data_mask(data))
}

### In the future we hope to not have to use this
### But right now I'm not sure how to evaluate
### data-mask specific functions like `n()` and friends
### so I'm relying to dplyr to do it
### Unfortunately I can't figure out a way to
### return a structure similar to the one
### eval_all_tidy() produces while keeping the expressions
### sequentially dependent
dplyr_eval_reframe <- function(data, ...){

  quos <- rlang::enquos(...)
  expr_names <- names(quos)
  group_vars <- group_vars(data)
  n_groups <- length(group_vars)

  results <- list()
  groups <- list()

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    #  Fix this later as new objects don't take precedence over data variables here
    if (nzchar(expr_name)){
      result <- dplyr::reframe(data, !!expr_name := !!quo)
    } else {
      result <- dplyr::reframe(data, !!quo)
    }
    result2 <- as.list(result)[seq_len(df_ncol(result) - n_groups) + n_groups]
    results <- c(results, result2)
    groups <- cheapr::list_combine(groups, cheapr::cheapr_rep(list(df_as_tbl(cheapr::sset_col(result, group_vars))), length(result2)))
  }
  names(groups) <- names(results)
  list(groups = groups, results = results)
}

dplyr_eval_summarise <- function(data, ...){

  quos <- rlang::enquos(...)
  expr_names <- names(quos)
  group_vars <- f_group_vars(data)
  n_groups <- length(group_vars)

  results <- list()

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    #  Fix this later as new objects don't take precedence over data variables here
    if (nzchar(expr_name)){
      result <- dplyr::summarise(data, !!expr_name := !!quo)
    } else {
      result <- dplyr::summarise(data, !!quo)
    }
    result2 <- as.list(result)[seq_len(df_ncol(result) - n_groups) + n_groups]
    results <- c(results, result2)
  }
  list(groups = f_group_keys(data), results = results)
}

eval_all_tidy <- function(data, quos, recycle = FALSE){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  GRP <- attr(quos, ".GRP", TRUE)
  optimised <- attr(quos, ".optimised", TRUE)
  group_unaware <- attr(quos, ".group_unaware", TRUE)

  if (is.null(GRP)){
    GRP <- grouped_df_as_GRP(f_ungroup(data))
    cheapr::attrs_modify(
      quos, .GRP = GRP, .set = TRUE
    )
  }

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)

  n_regular <- length(which_regular)
  n_group_aware_optimised <- cheapr::val_count(group_unaware, FALSE)
  n_group_unaware <- cheapr::val_count(group_unaware, TRUE)

  # If slow_recycle is true then we have to do a manual recycling of results
  slow_recycle <- recycle && length(which_optimised) > 0 &&
    length(which_regular) > 0

  slow_recycle <- slow_recycle || (n_group_unaware > 0 && n_group_aware_optimised > 0)

  if (slow_recycle){
    backup_GRP <- GRP

    GRP[["groups"]] <- cheapr::new_df(
      .internal.x = cheapr::sset(
        GRP_group_id(GRP), GRP_starts(GRP)
      )
    )
    GRP[["group.vars"]] <- ".internal.x"

    cheapr::attrs_modify(
      quos, .GRP = GRP, .set = TRUE
    )
    data <- cheapr::df_modify(
      data, list(.internal.x = GRP_group_id(GRP))
    )
  }

  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_all_tidy_optimised_quos(data, optimised_quos)

  inform_user_on_eval_split(regular_quos, optimised_quos)

  if (cpp_any_quo_contains_dplyr_mask_call(regular_quos)){
    regular_results <- dplyr_eval_reframe(
      construct_fastplyr_grouped_df(data, GRP), !!!regular_quos
    )
  } else {
    # Doing this check to potentially avoid calculating group locations
    # through a call to `construct_fastplyr_grouped_df()`
    # which will happen in e.g. `f_reframe(df, new = mean(x), .by = g)`
    if (length(regular_quos) == 0){
      regular_results <- list(
        groups = `names<-`(list(), character()),
        results = `names<-`(list(), character())
      )
    } else if (length(optimised_quos) == 0){
      regular_results <- cpp_grouped_eval_tidy(
        construct_fastplyr_grouped_df(data, GRP), regular_quos,
        recycle = recycle, add_groups = TRUE
      )
    } else {
      # Don't recycle in this case as we will manually recycle later
      regular_results <- cpp_grouped_eval_tidy(
        construct_fastplyr_grouped_df(data, GRP), regular_quos,
        recycle = FALSE, add_groups = TRUE
      )
    }
  }

  groups <- cheapr::new_list(length(quos))
  groups[which_regular] <- regular_results[[1]]
  groups[which_optimised] <- optimised_results[[1]]
  names(groups) <- quo_names

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results[[2]]
  results[which_optimised] <- optimised_results[[2]]
  names(results) <- quo_names

  # If the user provides expressions that can be optimised alongside
  # ones which can't, then special by-group recycling needs to occur
  # to ensure that each within-group result is recycled correctly

  if (slow_recycle){

    group_keys <- construct_group_keys(data, backup_GRP)
    grps <- lapply(groups, \(df) GRP2(
      df[[1L]], return.locs = FALSE, sort = FALSE, return.order = FALSE,
      return.groups = FALSE
    ))

    sizes <- lapply(grps, GRP_group_sizes)

    all_sizes_the_same <- TRUE

    for (i in 2:length(sizes)){
      all_sizes_the_same <- all_sizes_the_same &&
        identical(sizes[[i]], sizes[[i - 1L]])
    }

    if (!all_sizes_the_same){

      # Split results by group

      # Add group locations
      grps <- lapply(grps, \(x){
        x[["locs"]] <- GRP_loc(x)
        x
      })

      split_results <- purrr::map2(results, grps, \(x, g) vec_group_split(x, g))
      split_results <- transpose_eval_results(split_results)
      split_results <- recycle_eval_results(split_results)
      split_results <- transpose_eval_results(split_results)
      recycled_sizes <- cheapr::list_lengths(split_results[[1]])
      results <- lapply(split_results, \(x) cheapr::cheapr_c(.args = x))
    } else {
      recycled_sizes <- sizes[[1]]
    }
    # These are group IDs of our recycled groups
    out_groups <- cheapr::seq_id(recycled_sizes)
    groups <- cheapr::sset(group_keys, out_groups)
    groups <- cheapr::cheapr_rep_len(
      list(groups), length(results)
    )
    names(groups) <- quo_names
  }

  # Unpack results

  k <- 1L
  for (i in seq_along(results)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(results[[k]])){
      results_to_append <- as.list(results[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      results <- append(results, results_to_append, after = k - 1L)
      groups <- append(groups, cheapr::cheapr_rep_len(groups[k], length(results_to_append)), after = k - 1L)
      names(groups) <- names(results)
      k <- k + length(results_to_append)
      results[[k]] <- NULL
      groups[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }

  list(groups = groups, results = results)
}

eval_summarise <- function(data, quos){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  optimised <- attr(quos, ".optimised", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)
  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_summarise_optimised_quos(data, optimised_quos)

  inform_user_on_eval_split(regular_quos, optimised_quos)

  if (cpp_any_quo_contains_dplyr_mask_call(regular_quos)){
    regular_results <- dplyr_eval_summarise(
      construct_fastplyr_grouped_df(data, GRP), !!!regular_quos
    )
  } else {
    # Doing this check to potentially avoid calculating group locations
    # through a call to `construct_fastplyr_grouped_df()`
    # which will happen in e.g. `f_summarise(df, new = mean(x), .by = g)`
    if (length(regular_quos) == 0){
      regular_results <- list(
        groups = construct_group_keys(data, GRP),
        results = `names<-`(list(), character())
      )
    } else {
      regular_results <- cpp_grouped_eval_summarise(
        construct_fastplyr_grouped_df(data, GRP), regular_quos
      )
    }
  }

  groups <- regular_results[[1]]

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results[[2]]
  results[which_optimised] <- optimised_results
  names(results) <- quo_names
  results <- unpack_results(results, quos)
  list(groups = groups, results = results)
}

eval_mutate <- function(data, quos){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  optimised <- attr(quos, ".optimised", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)
  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_mutate_optimised_quos(data, optimised_quos)

  inform_user_on_eval_split(regular_quos, optimised_quos)

  if (cpp_any_quo_contains_dplyr_mask_call(regular_quos)){
    regular_results <- as.list(dplyr::mutate(
      construct_fastplyr_grouped_df(data, GRP), !!!regular_quos
    ))[names(regular_quos)]
  } else {
    # Doing this check to potentially avoid calculating group locations
    # through a call to `construct_fastplyr_grouped_df()`
    # which will happen in e.g. `f_mutate(df, new = mean(x), .by = g)`
    if (length(regular_quos) == 0){
      regular_results <- `names<-`(list(), character())
    } else {
      regular_results <- cpp_grouped_eval_mutate(
        construct_fastplyr_grouped_df(data, GRP), regular_quos
      )
    }
  }

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results
  results[which_optimised] <- optimised_results
  names(results) <- quo_names
  results <- unpack_results(results, quos)
  results
}

# tidy_overview <- function(x, ...){
#   ovrvw <- overview(x, ...)
#   for (i in seq_along(ovrvw)){
#     item <- ovrvw[[i]]
#     if (is_df(item)){
#       ovrvw[[i]] <- as_tbl(item)
#     }
#   }
#   class(ovrvw) <- c("tidy_overview", class(ovrvw))
#   ovrvw
# }
# print.tidy_overview <- function (x, max = NULL, ...) {
#   temp <- unclass(x)
#   for (i in seq_along(temp)) {
#     if (inherits(temp[[i]], "data.frame")) {
#       temp[[i]][["class"]] <- NULL
#     }
#   }
#   cat(paste("obs:", temp$obs, "\ncols:", temp$cols), "\n")
#   if (nrow(temp$logical)) {
#     cat("\n----- Logical -----\n")
#     print(temp$logical)
#   }
#   if (nrow(temp$numeric)) {
#     cat("\n----- Numeric -----\n")
#     print(temp$numeric)
#   }
#   if (nrow(temp$date)) {
#     cat("\n----- Dates -----\n")
#     print(temp$date)
#   }
#   if (nrow(temp$datetime)) {
#     datetime_chr_min <- character(nrow(temp$datetime))
#     datetime_chr_max <- character(nrow(temp$datetime))
#     mins <- temp[["datetime"]][["min"]]
#     maxs <- temp[["datetime"]][["max"]]
#     tzones <- temp[["datetime"]][["tzone"]]
#     for (i in seq_len(nrow(temp$datetime))) {
#       datetime_chr_min[i] <- format(mins[i], tz = tzones[i])
#       datetime_chr_max[i] <- format(maxs[i], tz = tzones[i])
#     }
#     temp$datetime$min <- datetime_chr_min
#     temp$datetime$max <- datetime_chr_max
#     cat("\n----- Date-Times -----\n")
#     print(temp$datetime)
#   }
#   if (nrow(temp$time_series)) {
#     cat("\n----- Time-Series -----\n")
#     temp$time_series$growth_rate <- paste0(pretty_num((x$time_series$growth_rate -
#                                                          1) * 100), "%")
#     print(temp$time_series)
#   }
#   if (nrow(temp$categorical)) {
#     cat("\n----- Categorical -----\n")
#     print(temp$categorical)
#   }
#   if (nrow(temp$other)) {
#     cat("\n----- Other -----\n")
#     print(temp$other)
#   }
#   invisible(x)
# }

as_list_of_ints <- function(x){
  vctrs_new_list_of(x, integer())
}
