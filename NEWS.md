# fastplyr (Development version)

* The `order` and `sort` argument for data frame functions has been superseded in 
favour of `.order` and `.sort`.

* `.order` arg has been added to `tidy_quantiles` to control the order of groups.

* New argument `.order` added to `f_summarise` to allow for controlling
the sort-order of groups.

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
