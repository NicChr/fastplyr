# fastplyr 0.5.1

- Small bug fix when `f_summarise` calculates means and medians 
for zero-row data frames with integer variables.

- R 4.0.0 now required.

# fastplyr 0.5.0

### Bug fixes

- Fix for `f_summarise` returning results in the incorrect order.

### Breaking changes

- Previously soft-deprecated arguments have now been removed.

### New features

- New function `list_tidy` as an alternative to `list` that evaluates 
arguments dynamically with a focus on setting precedence for objects created
in the list over environment objects.

- `new_tbl` now evaluates its arguments dynamically. `f_expand` also 
evaluates its argument dynamically unless the data is grouped and the 
expressions supplied aren't simply column selections.

- New function `f_pull` as a fast convenience function for extracting 
vectors from columns.

- New functions `remove_rows_if_any_na` and `remove_rows_if_all_na`.

- `f_arrange` gains the `.descending` argument to efficiently 
return data frames in descending order.

# fastplyr 0.4.0

### New features

- New function `f_fill` to fill `NA` values forwards and backwards by group.

### Improvements

- `f_bind_rows` sees a noticeable speed improvement.

### Bug fixes

- `f_summarise` now returns results in the correct order when both 
multiple cols and multiple optimised functions were specified.

- Joins were returning an error when `x` and `y` are `grouped_df` objects.

- The join by argument now accepts a partial named 
character vector without throwing an error.

- `tidy_quantiles` would return an error when probabilities were not sorted and
has now been fixed.

### Breaking changes

- The `seed` argument of `f_slice_sample` is soft-deprecated. To achieve 
sampling, or really any RNG functions with a local seed, 
use `cheapr::with_local_seed()`.

# fastplyr 0.3.0

* `tidy_quantiles` gains dramatic speed and efficiency improvements.

* The `order` and `sort` arguments for data frame functions have been 
superseded in favour of `.order` and `.sort`.

* New argument `.order` added to both `f_summarise` and `tidy_quantiles` 
to allow for controlling the order of groups.

* `rowwise_df` is now explicitly unsupported. To group by row, use `f_rowwise`.

* New functions `f_nest_by`, `f_rowwise` and `add_consecutive_id`.

# fastplyr 0.2.0

* A few bug fixes including: 
  * `f_bind_rows` was not working when supplied with more than 2 data frames in
some cases.
  * `f_summarise` was not working when supplied with non-function expressions.
  * A rare bug that caused R to crash has now been likely fixed.


* `f_bind_cols` now recycles its arguments and converts non-data frames
to data frames to allow for joining variables as if they were columns.

* Fixed a bug in the `f_join` functions where incorrect matches were 
occurring when the columns being joined on are 'exotic' variables, e.g. 
lists, lubridate 'Intervals', etc. Currently fastplyr uses a proxy method to 
join these kinds of variables through the use of `group_id`. This was not being
applied correctly for joined exotic variables and should now be fixed.

* New function `f_consecutive_id` as an alternative to `dplyr::consecutive_id`.

# fastplyr 0.1.0 (12-Sep-2024)

* Initial CRAN submission.
