#' Bind data frame rows and columns
#'
#' @description
#' Faster bind rows and columns.
#'
#' @param ... Data frames to bind.
#' @param .repair_names Should duplicate column names be made unique?
#' Default is `TRUE`.
#' @param .sep Separator to use for creating unique column names.
#'
#' @returns
#' `f_bind_rows()` performs a union of the data frames specified via `...` and
#' joins the rows of all the data frames, without removing duplicates.
#'
#' `f_bind_cols` joins the columns, creating unique column names if there are
#' any duplicates by default.
#'
#' @rdname f_bind_rows
#' @export
f_bind_rows <- function(...){
  dots <- list3(...)
  n_dots <- length(dots)
  ncols <- cpp_ncols(dots, TRUE)
  if (n_dots == 0){
    new_df()
  } else if (n_dots == 1){
    dots[[1L]]
  } else {
    template <- dots[[1L]]
    template_names <- names(template)
    dots <- lapply(
      dots, function(x) f_select(df_ungroup(x), .cols = template_names)
    )
    if (!cpp_any_frames_exotic(dots)){

      # We can use collapse::rowbind if data frames
      # contain simple atomic vectors

      rowbind <- function(...){
        collapse::rowbind(..., return = "data.frame")
      }
      reconstruct(template, do.call(rowbind, dots))
    } else {

      # Combine each variable separately

      out <- new_df(.nrows = sum(cpp_nrows(dots, FALSE)))
      for (j in seq_len(ncols[1])){
        temp <- vector("list", length(dots))
        for (i in seq_along(dots)){
          temp[[i]] <- dots[[i]][[j]]
        }
        out[[j]] <- do.call(f_union_all, temp)
        names(out)[j] <- template_names[[j]]
      }
      reconstruct(template, out)
    }
  }
}
#' @rdname f_bind_rows
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .sep = "..."){
  dots <- list3(...)
  nrows <- cpp_nrows(dots, TRUE)
  out <- unlist(unname(dots), recursive = FALSE)
  if (.repair_names){
    names(out) <- unique_name_repair(names(out), .sep = .sep)
  }
  out <- list_as_df(out)
  if (length(dots) == 1){
    out <- dots[[1L]]
    if (!is_df(out)){
      stop("All inputs must be data frames")
    }
  } else if (length(dots) > 1){
    N <- nrows[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
    template <- dots[[1L]]

    # Special method for grouped_df because
    # we don't need to recalculate groups
    # Since we're not rearranging or renaming variables
    # except in the case of duplicates.

    if (inherits(template, "grouped_df") &&
        all(group_vars(template) %in% names(out))){
      out <- reconstruct(df_ungroup(template), out)
      class(out) <- class(template)
      attr(out, "groups") <- attr(template, "groups")
    } else {
      out <- reconstruct(template, out)
    }
  }
  out
}
