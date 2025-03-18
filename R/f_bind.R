#' Bind data frame rows and columns
#'
#' @description
#' Faster bind rows and columns.
#'
#' @param ... Data frames to bind.
#' @param .repair_names Should duplicate column names be made unique?
#' Default is `TRUE`.
#' @param .sep Separator to use for creating unique column names.
#' @param .fill Should missing columns be filled with `NA`?
#' Default is `TRUE`.
#' @param .recycle Should inputs be recycled to a common row size?
#' Default is `TRUE`.
#'
#' @returns
#' `f_bind_rows()` performs a union of the data frames specified via `...` and
#' joins the rows of all the data frames, without removing duplicates.
#'
#' `f_bind_cols()` joins the columns, creating unique column names if there are
#' any duplicates by default.
#'
#' @rdname f_bind_rows
#' @export
f_bind_rows <- function(..., .fill = TRUE){
  dots <- tidy_as_list_of(...)
  n_dots <- length(dots)
  dims <- cpp_frame_dims(dots, check_rows_equal = FALSE, check_cols_equal = !.fill)
  nrows <- dims[[1L]]
  ncols <- dims[[2L]]


  if (n_dots == 0){
    new_tbl()
  } else if (n_dots == 1){
    dots[[1L]]
  } else if (sum(ncols) == 0){
    return(reconstruct(dots[[1L]], cheapr::new_df(.nrows = sum(nrows))))
  } else {
    template <- dots[[1L]]
    prototype_names <- names(template)

    fast_rowbind <- function(...){
      collapse::rowbind(..., return = "data.frame", fill = .fill)
    }

    if (!cpp_any_frames_exotic(dots)){

      # We can use collapse::rowbind if data frames
      # contain simple atomic vectors
      reconstruct(template, fast_rowbind(dots))
    } else {
      col_prototypes <- as.list(cheapr::sset(template, 0))

      # Standardise all frames to have the same cols in the same order

      if (.fill){
        get_prototype <- function(x) cheapr::sset(x, 0L)
        for (df in dots){
          new_prototype_names <- fast_setdiff(names(df), prototype_names)
          if (length(new_prototype_names)){
            prototype_names <- c(prototype_names, new_prototype_names)
            temp_df <- as.list(df)[new_prototype_names]
            col_prototypes <- c(
              col_prototypes, lapply(
                temp_df,
                get_prototype
              )
            )
          }
        }

        # Second pass to fill in missing cols

        for (i in seq_along(dots)){
          df <- dots[[i]]
          cols_to_add <- fast_setdiff(prototype_names, names(df))
          if (length(cols_to_add)){
            df_to_bind <- list_as_df(lapply(col_prototypes[cols_to_add], na_init, nrows[[i]]))
            df <- fast_bind_cols(df, df_to_bind)
          }
          dots[[i]] <- cheapr::sset_col(df, prototype_names)
        }
      }

      # Now that frames are standardised
      # we perform the join
      # by using fast_rowbind on the simple atomic cols
      # and `c()` on everything else

      out_ncols <- length(col_prototypes)
      out_nrows <- sum(nrows)
      out <- cheapr::new_list(out_ncols)
      names(out) <- names(col_prototypes)

      # Exotic variables

      is_exotic <- vapply(col_prototypes, cpp_is_exotic, FALSE, USE.NAMES = FALSE)
      exotic_vars <- names(col_prototypes)[cheapr::which_(is_exotic)]

      # All other variables

      other_vars <- names(col_prototypes)[cheapr::which_(is_exotic, invert = TRUE)]

      out_nms <- names(col_prototypes)
      out_nrows <- sum(nrows)

      exotic_ncols <- sum(is_exotic)
      exotic_out <- cheapr::new_list(exotic_ncols)
      names(exotic_out) <- exotic_vars

      # Use fast_rowbind on non-exotic variables

      if (length(other_vars) == 0){
        other_out <- new_tbl(.nrows = out_nrows)
      } else {
        other_out <- do.call(
          fast_rowbind,
          c(lapply(dots, cheapr::sset_col, other_vars),
            list(use.names = FALSE))
        )
      }


      # For exotic variables, we use `c()` and rely on S3 methods
      # Except for data frames, in which case `f_bind_rows()` is called
      # recursively

      for (j in seq_len(exotic_ncols)){
        temp <- cheapr::new_list(length(dots))
        for (i in seq_along(dots)){
          cpp_set_list_element(temp, i, dots[[i]][[exotic_vars[[j]]]])
        }
        exotic_out[[j]] <- do.call(combine, temp)
      }

      attr(exotic_out, "row.names") <- .set_row_names(out_nrows)
      class(exotic_out) <- "data.frame"

      # Bind both data frames together and select them in the right order

      out <- f_select(f_bind_cols(other_out, exotic_out, .recycle = FALSE), .cols = out_nms)
      reconstruct(template, out)
    }
  }
}
#' @rdname f_bind_rows
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .recycle = TRUE, .sep = "..."){
  dots <- tidy_as_list_of(...)
  if (.recycle){
    dots <- do.call(cheapr::recycle, dots)
  }

  dots <- cpp_as_list_of_frames(dots)

  nrows <- cpp_frame_dims(dots, check_rows_equal = !.recycle, check_cols_equal = FALSE)[[1L]]
  out <- as.list(unlist(unname(dots), recursive = FALSE))
  if (.repair_names){
    names(out) <- unique_name_repair(names(out), .sep = .sep)
    if (is.null(names(out))){
      if (length(out) == 0){
        names(out) <- character()
      } else {
        names(out) <- paste0(.sep, seq_along(out))
      }
    }
  }
  out <- list_as_tbl(out)
  if (length(dots) >= 1){
    N <- nrows[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
    out <- reconstruct(dots[[1L]], out)
  }
  out
}
