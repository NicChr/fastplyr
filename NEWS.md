# fastplyr (Development version)

* Fixed a bug in the `f_join` functions where incorrect matches were 
occurring when the columns being joined on are 'exotic' variables, e.g. 
lists, lubridate 'Intervals', etc. Currently fastplyr uses a proxy method to 
join these kinds of variables through the use of `group_id`. This was not being
applied correctly for joined exotic variables and should now be fixed.

* New function `f_consecutive_id` as an alternative to `dplyr::consecutive_id`.

# fastplyr 0.1.0 (12-Sep-2024)

* Initial CRAN submission.
