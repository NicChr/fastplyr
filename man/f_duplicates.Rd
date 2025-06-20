% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_duplicates.R
\name{f_duplicates}
\alias{f_duplicates}
\title{Find duplicate rows}
\usage{
f_duplicates(
  data,
  ...,
  .keep_all = FALSE,
  .both_ways = FALSE,
  .add_count = FALSE,
  .drop_empty = FALSE,
  .order = FALSE,
  .sort = deprecated(),
  .by = NULL,
  .cols = NULL
)
}
\arguments{
\item{data}{A data frame.}

\item{...}{Variables used to find duplicate rows.}

\item{.keep_all}{If \code{TRUE} then all columns of data frame are kept,
default is \code{FALSE}.}

\item{.both_ways}{If \code{TRUE} then duplicates and non-duplicate first instances
are retained. The default is \code{FALSE} which returns only duplicate rows. \cr
Setting this to \code{TRUE} can be particularly useful when examining
the differences between duplicate rows.}

\item{.add_count}{If \code{TRUE} then a count column is added to denote the
number of duplicates (including first non-duplicate instance).
The naming convention of this column follows \code{dplyr::add_count()}.}

\item{.drop_empty}{If \code{TRUE} then empty rows with all \code{NA} values are removed.
The default is \code{FALSE}.}

\item{.order}{Should the groups be calculated as ordered groups?
Setting to \code{TRUE} here implies that the groups are returned sorted.}

\item{.sort}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}  Use \code{.order} instead.}

\item{.by}{(Optional). A selection of columns to group by for this operation.
Columns are specified using tidy-select.}

\item{.cols}{(Optional) alternative to \code{...} that accepts
a named character vector or numeric vector.
If speed is an expensive resource, it is recommended to use this.}
}
\value{
A \code{data.frame} of duplicate rows.
}
\description{
Find duplicate rows
}
\details{
This function works like \code{dplyr::distinct()} in its handling of
arguments and data-masking but returns duplicate rows.
In certain situations in can be much faster than \code{data |> group_by()|> filter(n() > 1)}
when there are many groups.
}
\seealso{
\link{f_count} \link{f_distinct}
}
