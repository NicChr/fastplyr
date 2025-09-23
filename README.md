
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastplyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/fastplyr)](https://CRAN.R-project.org/package=fastplyr)
[![Codecov test
coverage](https://codecov.io/gh/NicChr/fastplyr/graph/badge.svg)](https://app.codecov.io/gh/NicChr/fastplyr)

<!-- badges: end -->

fastplyr aims to provide a [tidyverse](https://www.tidyverse.org/learn)
frontend using a
[collapse](https://sebkrantz.github.io/collapse/articles/collapse_intro.html)
backend. This means from a user’s point of view the functions behave
like the tidyverse equivalents and thus require little to no changes to
existing code to convert.

fastplyr is designed to handle operations that involve larger numbers of
groups and generally larger data.

## Installation

You can install the development version of fastplyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("NicChr/fastplyr")
```

Load packages

``` r
library(tidyverse)
#> Warning: package 'ggplot2' was built under R version 4.5.1
#> Warning: package 'tibble' was built under R version 4.5.1
#> Warning: package 'purrr' was built under R version 4.5.1
#> Warning: package 'stringr' was built under R version 4.5.1
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.2
#> ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.1.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(fastplyr)
#> 
#> Attaching package: 'fastplyr'
#> 
#> The following object is masked from 'package:dplyr':
#> 
#>     desc
#> 
#> The following objects are masked from 'package:tidyr':
#> 
#>     crossing, nesting
library(nycflights13)
library(bench)
```

While the syntax and user-interface of fastplyr aligns very closely with
dplyr most of the time, there can be a few key differences.

## Differences between fastplyr and dplyr

<table style="width:94%;">
<colgroup>
<col style="width: 23%" />
<col style="width: 30%" />
<col style="width: 40%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><h3 id="dplyr">dplyr</h3></th>
<th><h3 id="fastplyr-1">fastplyr</h3></th>
</tr>
</thead>
<tbody>
<tr>
<td><code>.by</code></td>
<td>Groups are sorted by order of first appearance always when using
<code>.by</code></td>
<td>Groups are always sorted by default, even when using
<code>.by</code>. One can use the other by setting
<code>.order = FALSE</code></td>
</tr>
<tr>
<td>Many groups</td>
<td>Generally slow for data with many groups.</td>
<td>Designed to be fast for data with many groups.</td>
</tr>
<tr>
<td>Handling of <code>...</code> expressions</td>
<td>Executes expressions in a way that latter expressions depend on
prior ones</td>
<td>Some expressions are executed independently to each other</td>
</tr>
<tr>
<td>Optimisations</td>
<td>Expressions are run by-group with minimal overhead, slow for many
groups</td>
<td>Many functions are optimised to either ignore groups or use faster
methods</td>
</tr>
<tr>
<td>Duplicate rows</td>
<td>No dedicated function for this, solution using
<code>group_ by |&gt; filter(n() &gt; 1)</code> are generally slow for
larger data.</td>
<td>Dedicated function <code>f_duplicates</code> can do this very fast
and with fine control.</td>
</tr>
<tr>
<td>Row slicing</td>
<td><code>slice()</code> supports data-masked expressions supplied to
<code>...</code></td>
<td>Data-masked expressions not supported in <code>f_slice_</code>
functions. Use <code>f_filter()</code> for this behaviour.</td>
</tr>
<tr>
<td>Memory usage</td>
<td>High memory usage</td>
<td>Lower usage compared to dplyr</td>
</tr>
<tr>
<td>joins</td>
<td>Accepts different types of joins, e.g. rolling and equality
joins.</td>
<td>Accepts only equality joins of the form <code>x == y</code></td>
</tr>
<tr>
<td>rowwise</td>
<td><code>rowwise_df</code> accepted and everything sub-setted implictly
using <code>[[</code></td>
<td><code>rowwise_df</code> not accepted, must use
<code>f_rowwise_df</code> which creates a <code>grouped_df</code> with a
row ID col. Implicit <code>[[</code> subsetting does not occur.</td>
</tr>
<tr>
<td>Matrices in data frames</td>
<td>Fully supported</td>
<td>Not supported</td>
</tr>
<tr>
<td>Grouped data frames</td>
<td>N/A</td>
<td><code>f_group_by</code> produces a <code>grouped_df</code> with some
additional metadata to assist with making later operations faster</td>
</tr>
</tbody>
</table>

## dplyr alternatives

All tidyverse alternative functions are prefixed with ‘f\_’. For
example, `dplyr::distinct` becomes `fastplyr::f_distinct`.

### distinct

``` r
flights |> 
  f_distinct(origin, dest)
#> ! The following functions will be optimised package-wide:
#> `sum`
#> `prod`
#> `mean`
#> `median`
#> `min`
#> `max`
#> `sd`
#> `var`
#> `dplyr::n`
#> `dplyr::first`
#> `dplyr::last`
#> `dplyr::n_distinct`
#> `dplyr::row_number`
#> `dplyr::lag`
#> `dplyr::lead`
#> `dplyr::cur_group`
#> `dplyr::cur_group_id`
#> `dplyr::cur_group_rows`
#> 
#> Optimised expressions are independent from unoptimised ones and typical
#> data-masking rules may not apply
#> 
#> Run `fastplyr::fastplyr_disable_optimisations()` to disable optimisations
#> globally
#> 
#> Run `fastplyr::fastplyr_disable_informative_msgs()` to disable this and other
#> informative messages
#> This message is displayed once per session.
#> # A tibble: 224 × 2
#>   origin dest 
#>   <chr>  <chr>
#> 1 EWR    IAH  
#> 2 LGA    IAH  
#> 3 JFK    MIA  
#> 4 JFK    BQN  
#> 5 LGA    ATL  
#> # ℹ 219 more rows
```

`f_distinct` has an additional `.order` argument which is much faster
than sorting afterwards.

``` r
mark(
  fastplyr_distinct_sort = flights |> 
  f_distinct(across(where(is.numeric)), .order = TRUE),
  dplyr_distinct_sort = flights |> 
    distinct(across(where(is.numeric))) |> 
    arrange_all()
)
#> # A tibble: 2 × 6
#>   expression                  min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>             <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_distinct_sort   18.1ms   19.2ms      52.0    33.1MB     63.6
#> 2 dplyr_distinct_sort      59.5ms   59.5ms      16.8    73.6MB    151.
```

### group_by

`f_group_by` operates very similarly with an additional feature that
allows you to specify whether group data should be ordered or not. This
ultimately controls if the groups end up sorted in expressions like
`count` and `summarise`, but also in this case `f_count` and
`f_summarise`.

``` r
# Like dplyr
flights |> 
  f_group_by(month) |> 
  f_count()
#> # A tibble: 12 × 2
#>   month     n
#>   <int> <int>
#> 1     1 27004
#> 2     2 24951
#> 3     3 28834
#> 4     4 28330
#> 5     5 28796
#> # ℹ 7 more rows

# Group data is sorted by order-of-first appearance
flights |> 
  f_group_by(month, .order = FALSE) |> 
  f_count()
#> # A tibble: 12 × 2
#>   month     n
#>   <int> <int>
#> 1     1 27004
#> 2    10 28889
#> 3    11 27268
#> 4    12 28135
#> 5     2 24951
#> # ℹ 7 more rows
```

Just a reminder that all fastplyr functions are interchangeable with
dplyr ones both ways

``` r

### With dplyr::count

flights |> 
  f_group_by(month) |> 
  count()
#> # A tibble: 12 × 2
#> # Groups:   month [12]
#>   month     n
#>   <int> <int>
#> 1     1 27004
#> 2     2 24951
#> 3     3 28834
#> 4     4 28330
#> 5     5 28796
#> # ℹ 7 more rows
```

``` r

### With dplyr::group_by

flights |> 
  group_by(month) |> 
  f_count()
#> # A tibble: 12 × 2
#>   month     n
#>   <int> <int>
#> 1     1 27004
#> 2     2 24951
#> 3     3 28834
#> 4     4 28330
#> 5     5 28796
#> # ℹ 7 more rows
```

### summarise

`f_summarise` behaves like dplyr’s `summarise` except for two things:

- It evaluates expressions independently
- There are optimisations for common statistical functions which are
  very fast for many groups

``` r
grouped_flights <- flights |> 
  group_by(across(where(is.character)))

grouped_flights |> 
  f_summarise(
    n = n(), mean_dep_delay = mean(dep_delay)
  )
#> # A tibble: 52,807 × 6
#>   carrier tailnum origin dest      n mean_dep_delay
#>   <chr>   <chr>   <chr>  <chr> <int>          <dbl>
#> 1 9E      N146PQ  JFK    ATL       8           9.62
#> 2 9E      N153PQ  JFK    ATL       5          -0.4 
#> 3 9E      N161PQ  JFK    ATL       3          -2   
#> 4 9E      N162PQ  EWR    DTW       1         160   
#> 5 9E      N162PQ  JFK    ATL       1          -6   
#> # ℹ 52,802 more rows
```

And a benchmark

``` r
mark(
  fastplyr_summarise = grouped_flights |> 
  f_summarise(
    n = n(), mean_dep_delay = mean(dep_delay)
  ),
  dplyr_summarise = grouped_flights |> 
  summarise(
    n = n(), mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    .groups = "drop"
  )
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_summarise   2.52ms   2.79ms    300.      3.58MB     11.9
#> 2 dplyr_summarise    538.62ms 538.62ms      1.86    9.59MB     20.4
```

### Joins

Joins work much the same way as in dplyr.

``` r
left <- flights |> 
  f_select(origin, dest, time_hour)
hours <- sample(unique(left$time_hour), 5000)
right <- as.data.frame(unclass(as.POSIXlt(hours)))
right$time_hour <- hours

# Left join

left |> 
  f_left_join(right)
#> # A tibble: 336,776 × 14
#>   origin dest  time_hour             sec   min  hour  mday   mon  year  wday
#>   <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
#> 1 EWR    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 2 LGA    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 3 JFK    MIA   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 4 JFK    BQN   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 5 LGA    ATL   2013-01-01 06:00:00     0     0     6     1     0   113     2
#> # ℹ 336,771 more rows
#> # ℹ 4 more variables: yday <int>, isdst <int>, zone <chr>, gmtoff <int>

# inner join

left |> 
  f_inner_join(right)
#> # A tibble: 244,029 × 14
#>   origin dest  time_hour             sec   min  hour  mday   mon  year  wday
#>   <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
#> 1 EWR    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 2 LGA    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 3 JFK    MIA   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 4 JFK    BQN   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 5 LGA    ATL   2013-01-01 06:00:00     0     0     6     1     0   113     2
#> # ℹ 244,024 more rows
#> # ℹ 4 more variables: yday <int>, isdst <int>, zone <chr>, gmtoff <int>

# Anti join

left |> 
  f_anti_join(right)
#> # A tibble: 92,747 × 3
#>   origin dest  time_hour          
#>   <chr>  <chr> <dttm>             
#> 1 LGA    ATL   2013-01-01 14:00:00
#> 2 LGA    ATL   2013-01-01 14:00:00
#> 3 EWR    ORD   2013-01-01 14:00:00
#> 4 EWR    SEA   2013-01-01 14:00:00
#> 5 EWR    ORD   2013-01-01 14:00:00
#> # ℹ 92,742 more rows

# Semi join

left |> 
  f_semi_join(right)
#> # A tibble: 244,029 × 3
#>   origin dest  time_hour          
#>   <chr>  <chr> <dttm>             
#> 1 EWR    IAH   2013-01-01 05:00:00
#> 2 LGA    IAH   2013-01-01 05:00:00
#> 3 JFK    MIA   2013-01-01 05:00:00
#> 4 JFK    BQN   2013-01-01 05:00:00
#> 5 LGA    ATL   2013-01-01 06:00:00
#> # ℹ 244,024 more rows

# full join

left |> 
  f_full_join(right)
#> # A tibble: 336,776 × 14
#>   origin dest  time_hour             sec   min  hour  mday   mon  year  wday
#>   <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
#> 1 EWR    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 2 LGA    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 3 JFK    MIA   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 4 JFK    BQN   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 5 LGA    ATL   2013-01-01 06:00:00     0     0     6     1     0   113     2
#> # ℹ 336,771 more rows
#> # ℹ 4 more variables: yday <int>, isdst <int>, zone <chr>, gmtoff <int>
```

And a benchmark comparing fastplyr and dplyr joins

``` r
mark(
  fastplyr_left_join = f_left_join(left, right, by = "time_hour"),
  dplyr_left_join = left_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_left_join   6.35ms   7.11ms     134.       18MB     29.5
#> 2 dplyr_left_join     20.16ms  21.91ms      45.5      45MB     45.5
```

``` r
mark(
  fastplyr_inner_join = f_inner_join(left, right, by = "time_hour"),
  dplyr_inner_join = inner_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_inner_join   4.63ms   5.37ms     180.     22.2MB     49.4
#> 2 dplyr_inner_join     16.07ms  17.06ms      58.0    37.9MB     39.9
```

``` r
mark(
  fastplyr_anti_join = f_anti_join(left, right, by = "time_hour"),
  dplyr_anti_join = anti_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_anti_join   2.16ms   2.48ms     386.     3.76MB     13.2
#> 2 dplyr_anti_join     11.44ms  13.05ms      75.4    21.8MB     12.6
```

``` r
mark(
  fastplyr_semi_join = f_semi_join(left, right, by = "time_hour"),
  dplyr_semi_join = semi_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_semi_join   3.33ms    3.6ms     260.      7.8MB     18.1
#> 2 dplyr_semi_join     11.21ms   13.3ms      76.2    21.9MB     22.6
```

``` r
mark(
  fastplyr_full_join = f_full_join(left, right, by = "time_hour"),
  dplyr_full_join = full_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_full_join   7.05ms   7.58ms     130.     19.3MB     40.4
#> 2 dplyr_full_join     20.33ms  21.85ms      45.1    44.6MB     60.2
```

### slice

`f_slice` and other `f_slice_` functions are very fast for many groups.

``` r
grouped_flights |> 
  f_slice(1)
#> # A tibble: 52,807 × 19
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#> 1  2013     1     7      614            615        -1      812            855
#> 2  2013     1     8      612            615        -3      901            855
#> 3  2013     1     9      615            615         0       NA            855
#> 4  2013     1    25     1530           1250       160     1714           1449
#> 5  2013     2    24      609            615        -6      835            855
#> # ℹ 52,802 more rows
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>

grouped_flights |>
  f_slice_head(3)
#> # A tibble: 125,770 × 19
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#> 1  2013     1     7      614            615        -1      812            855
#> 2  2013     1    13      612            615        -3      853            855
#> 3  2013     2     3      617            615         2      902            855
#> 4  2013     1     8      612            615        -3      901            855
#> 5  2013     1    22      614            615        -1      857            855
#> # ℹ 125,765 more rows
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

A quick benchmark to prove the point

``` r
mark(
    fastplyr_slice = grouped_flights |> 
    f_slice_head(n = 3),
    dplyr_slice = grouped_flights |>
        slice_head(n = 3)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression          min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_slice  18.42ms  22.58ms    43.4      23.8MB     9.85
#> 2 dplyr_slice       2.35s    2.35s     0.425    26.6MB    25.9
```

### Group metadata

Group metadata helpers like `cur_group_id()` get optimised in `f_mutate`

``` r

## Unique ID for each group

mark(
  dplyr = grouped_flights |> 
  f_mutate(group_id = cur_group_id(), .keep = "none"),
  fastplyr = grouped_flights |> 
  mutate(group_id = cur_group_id(), .keep = "none")
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 dplyr         1.4ms   1.93ms    480.       3.2MB     7.97
#> 2 fastplyr      227ms 230.56ms      4.36    3.21MB    20.3
```

### expand

Based closely on `tidyr::expand`, `f_expand()` can cross joins multiple
vectors and data frames.

``` r
mark(
    fastplyr_expand = flights |> 
        f_group_by(origin, tailnum) |> 
        f_expand(month = 1:12),
    tidyr_expand = flights |> 
        group_by(origin, tailnum) |> 
        expand(month = 1:12),
    check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_expand  15.21ms  17.74ms    30.9      11.7MB     6.87
#> 2 tidyr_expand       3.04s    3.04s     0.329    81.1MB     5.60


# Using `.cols` in `f_expand()` is very fast!
mark(
    fastplyr_expand = flights |> 
        f_group_by(origin, dest) |> 
        f_expand(.cols = c("year", "month", "day")),
    tidyr_expand = flights |> 
        group_by(origin, dest) |> 
        expand(year, month, day),
    check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_expand   11.3ms   12.7ms     70.8     16.8MB    15.7 
#> 2 tidyr_expand     164.7ms    185ms      4.38    66.7MB     5.84
```

### duplicate rows

Finding duplicate rows is a very common dataset operation and there is a
dedicated function `f_duplicates()` to do exactly this.

``` r
flights |> 
  f_duplicates(time_hour)
#> # A tibble: 329,840 × 1
#>   time_hour          
#>   <dttm>             
#> 1 2013-01-01 05:00:00
#> 2 2013-01-01 05:00:00
#> 3 2013-01-01 05:00:00
#> 4 2013-01-01 05:00:00
#> 5 2013-01-01 06:00:00
#> # ℹ 329,835 more rows
```

Benchmark against a common dplyr strategy for finding duplicates

``` r
mark(
 fastplyr_duplicates = flights |> 
   f_duplicates(time_hour, .both_ways = TRUE, .add_count = TRUE, .keep_all = TRUE),
 dplyr_duplicates = flights |> 
   add_count(time_hour) |> 
   filter(n > 1)
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_duplicates   10.3ms   10.6ms      91.7    45.1MB     86.3
#> 2 dplyr_duplicates      45.9ms   47.7ms      21.0    59.5MB     25.2
```

### filter

In the worst-case scenarios, `f_filter()` is about the same speed as
`filter()` and in the best-case is much faster and more efficient. This
is especially true for large data where small subsets of the data are
returned.

``` r
full <- new_tbl(x = rnorm(5e07))

# A worst case scenario

mark(
  fastplyr_filter = full |> 
    f_filter(abs(x) > 0),
  dplyr_filter = full |> 
    filter(abs(x) > 0)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_filter    691ms    691ms      1.45    1.12GB     1.45
#> 2 dplyr_filter       708ms    708ms      1.41    1.68GB     4.24

# Best case scenario - filter results in small subset

mark(
  fastplyr_filter = full |> 
    f_filter(x > 4),
  dplyr_filter = full |> 
    filter(x > 4)
)
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_filter    172ms    177ms      5.66     191MB     2.83
#> 2 dplyr_filter       344ms    344ms      2.90     763MB     2.90
```

### bind rows and cols

Binding columns is particular much faster but binding rows is also
sufficiently faster

``` r
mark(
  fastplyr_bind_cols = f_bind_cols(grouped_flights, grouped_flights),
  dplyr_bind_cols = suppressMessages(
    bind_cols(grouped_flights, grouped_flights)
    ),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_bind_cols   32.2µs   36.9µs  22606.     42.19KB     2.26
#> 2 dplyr_bind_cols     175.2ms  188.2ms      5.35    1.02MB     7.13

mark(
  fastplyr_bind_rows = f_bind_rows(grouped_flights, grouped_flights),
  dplyr_bind_rows = bind_rows(grouped_flights, grouped_flights)
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_bind_rows   48.2ms   53.1ms     19.2       86MB     2.14
#> 2 dplyr_bind_rows       161ms    163ms      6.05     158MB     0
```

### Quantiles

A typical tidy approach might use a mixture of `reframe()` and
`enframe()` which is a perfectly tidy and neat solution

``` r
probs <- seq(0, 1, 0.25)

mtcars <- as_tbl(mtcars)

mtcars |> 
 group_by(cyl) |> 
 reframe(enframe(quantile(mpg, probs), "quantile", "mpg"))
#> # A tibble: 15 × 3
#>     cyl quantile   mpg
#>   <dbl> <chr>    <dbl>
#> 1     4 0%        21.4
#> 2     4 25%       22.8
#> 3     4 50%       26  
#> 4     4 75%       30.4
#> 5     4 100%      33.9
#> # ℹ 10 more rows
```

fastplyr though has a dedicated function for quantile calculation,
`tidy_quantiles()` which requires less code to type

``` r

# Wide
mtcars |> 
  tidy_quantiles(mpg, .by = cyl, pivot = "wide")
#> # A tibble: 3 × 6
#>     cyl    p0   p25   p50   p75  p100
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     4  21.4  22.8  26    30.4  33.9
#> 2     6  17.8  18.6  19.7  21    21.4
#> 3     8  10.4  14.4  15.2  16.2  19.2

# Long
mtcars |> 
  tidy_quantiles(mpg, .by = cyl, pivot = "long")
#> # A tibble: 15 × 3
#>     cyl .quantile   mpg
#>   <dbl> <fct>     <dbl>
#> 1     4 p0         21.4
#> 2     4 p25        22.8
#> 3     4 p50        26  
#> 4     4 p75        30.4
#> 5     4 p100       33.9
#> # ℹ 10 more rows
```

Not only can you choose how to pivot as shown above, you can also
calculate quantiles for multiple variables.

``` r
multiple_quantiles <- mtcars |> 
  tidy_quantiles(across(where(is.numeric)), pivot = "long")
multiple_quantiles
#> # A tibble: 5 × 12
#>   .quantile   mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 p0         10.4     4  71.1  52    2.76  1.51  14.5     0     0     3     1
#> 2 p25        15.4     4 121.   96.5  3.08  2.58  16.9     0     0     3     2
#> 3 p50        19.2     6 196.  123    3.70  3.32  17.7     0     0     4     2
#> 4 p75        22.8     8 326   180    3.92  3.61  18.9     1     1     4     4
#> 5 p100       33.9     8 472   335    4.93  5.42  22.9     1     1     5     8

# Quantile names is a convenient factor
multiple_quantiles$.quantile
#> [1] p0   p25  p50  p75  p100
#> Levels: p0 p25 p50 p75 p100
```

### Quantile benchmark for many groups

`tidy_quantiles()` of course is fast when many groups are involved.

``` r
mark(
  fastplyr_quantiles = flights |> 
  tidy_quantiles(dep_delay, pivot = "long",
                 .by = c(year, month, day, origin)),
  dplyr_quantiles = flights |> 
     group_by(year, month, day, origin) |> 
    reframe(enframe(quantile(dep_delay, seq(0, 1, 0.25), na.rm = TRUE))),
  check = FALSE
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_quantiles     20ms   21.8ms     45.2     4.27MB     2.06
#> 2 dplyr_quantiles       140ms  139.8ms      7.15   24.98MB    21.5
```

### Details on internally optimised functions

fastplyr categorises all expressions into one of 3 categories

1.  Standard expressions
2.  Group-unaware expressions
3.  Group-aware optimisable expressions

The first category are normal expressions which simply don’t belong to
the other 2 categories and are evaluated normally.

The second category consists of group-unaware expressions. These can be
be evaluated once on the entire data instead of by-group. For example
the plus function `+` is group-unaware.

The third category consists of functions that are group-aware but can be
optimised, such as most of the common statistical functions like `sum`,
`mean`, etc.

### Group-unaware functions

Some common base R functions can be thought of as group-unaware in the
sense that they return the same results regardless of if they are called
in a grouped context.

fastplyr evaluates these functions once as if there are no groups.

Current list of functions marked as group-unaware

``` r
fns <- get_group_unaware_fns()

names(fns)
#>  [1] "|"        "&"        "!"        ">="       ">"        "<="      
#>  [7] "<"        "=="       "!="       "%%"       "%/%"      "+"       
#> [13] "-"        "*"        "/"        "^"        "abs"      "sign"    
#> [19] "floor"    "trunc"    "round"    "signif"   "exp"      "log"     
#> [25] "("        "{"        "expm1"    "log1p"    "cos"      "sin"     
#> [31] "tan"      "cospi"    "sinpi"    "tanpi"    "acos"     "asin"    
#> [37] "atan"     "cosh"     "sinh"     "tanh"     "acosh"    "asinh"   
#> [43] "atanh"    "lgamma"   "gamma"    "digamma"  "trigamma" "identity"
#> [49] "gcd2"     "scm2"

# base::round for example
fns$round
#> function (x, digits = 0, ...)  .Primitive("round")
```

An expression is marked as group-unaware if and only if all calls in the
call-tree are group-unaware.

``` r

# Group-unaware fn names
fn_names <- names(fns)

expr <- quote(x - y)
rlang::is_call(expr, "-")
#> [1] TRUE

expr <- quote(x - y + z)

# Top-level expr is a group-unaware call
rlang::is_call(expr, "+")
#> [1] TRUE

# `-` expression nested inside is also group-unaware
expr |> 
  as.list() |> 
  pluck(2) |> 
  print() |> 
  rlang::is_call(fn_names)
#> x - y
#> [1] TRUE

# Definitely group-aware as `sum()` depends on the group-context
expr <- quote(sum(x - y))
rlang::is_call(expr, fn_names)
#> [1] FALSE
```

This allows us to write out more complex expressions and evaluate them
very efficiently

``` r
mark(
    fastplyr = grouped_flights |> 
        f_mutate(x = round(abs(arr_time - dep_time)), .keep = "none"), 
    dplyr = grouped_flights |> 
        mutate(x = round(abs(arr_time - dep_time)), .keep = "none")
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr       13ms   13.7ms     70.7     9.61MB     0   
#> 2 dplyr         147ms  157.2ms      6.17    8.89MB     7.72
```

### Group-aware optimised functions

fastplyr also optimises many common statistical functions like `sum`,
`mean` for use on large grouped data frames.

A list of currently optimised group-aware functions can be viewed in
`f_summarise.Rd` or by running `?f_summarise` in Rstudio.

``` r
res <- grouped_flights |> 
  f_summarise(across(where(is.numeric), mean)) |> 
  mark()
res$result;res
#> [[1]]
#> # A tibble: 52,807 × 18
#>   carrier tailnum origin dest   year month   day dep_time sched_dep_time
#>   <chr>   <chr>   <chr>  <chr> <dbl> <dbl> <dbl>    <dbl>          <dbl>
#> 1 9E      N146PQ  JFK    ATL    2013  1.75 11.9      630.            615
#> 2 9E      N153PQ  JFK    ATL    2013  1.6  16        615.            615
#> 3 9E      N161PQ  JFK    ATL    2013  1.33  8.67     613             615
#> 4 9E      N162PQ  EWR    DTW    2013  1    25       1530            1250
#> 5 9E      N162PQ  JFK    ATL    2013  2    24        609             615
#> # ℹ 52,802 more rows
#> # ℹ 9 more variables: dep_delay <dbl>, arr_time <dbl>, sched_arr_time <dbl>,
#> #   arr_delay <dbl>, flight <dbl>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>
#> # A tibble: 1 × 6
#>   expression                             min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                          <bch:> <bch:>     <dbl> <bch:byt>    <dbl>
#> 1 f_summarise(grouped_flights, acros… 19.1ms 20.3ms      48.7    8.85MB        0
```

Other group-aware functions that fastplyr optimises include dplyr group
metadata functions like `n()`, `row_number()`, `cur_group_id()`, etc.

``` r
grouped_flights |> 
  f_mutate(
    n = n(),
    row_id = row_number(),
    group_id = cur_group_id(),
    group_locs = cur_group_rows(),
    .keep = "none"
  )
#> # A tibble: 336,776 × 8
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>   carrier tailnum origin dest      n row_id group_id group_locs
#>   <chr>   <chr>   <chr>  <chr> <int>  <int>    <int>      <int>
#> 1 UA      N14228  EWR    IAH       8      1    35951          1
#> 2 UA      N24211  LGA    IAH       3      1    36937          2
#> 3 AA      N619AA  JFK    MIA      11      1     8489          3
#> 4 B6      N804JB  JFK    BQN       2      1    15462          4
#> 5 DL      N668DN  LGA    ATL      38      1    20325          5
#> # ℹ 336,771 more rows
```

Lags and leads are also optimised by-group

``` r
flights |> 
  f_mutate(
    time_hour,
    lag = lag(time_hour),
    lead = lead(time_hour),
    .by = origin,
    .keep = "none"
  )
#> # A tibble: 336,776 × 4
#>   origin time_hour           lag                 lead               
#>   <chr>  <dttm>              <dttm>              <dttm>             
#> 1 EWR    2013-01-01 05:00:00 NA                  2013-01-01 05:00:00
#> 2 LGA    2013-01-01 05:00:00 NA                  2013-01-01 06:00:00
#> 3 JFK    2013-01-01 05:00:00 NA                  2013-01-01 05:00:00
#> 4 JFK    2013-01-01 05:00:00 2013-01-01 05:00:00 2013-01-01 06:00:00
#> 5 LGA    2013-01-01 06:00:00 2013-01-01 05:00:00 2013-01-01 06:00:00
#> # ℹ 336,771 more rows
```

The caveat about this approach is that the usual behaviour of
expressions being able to reference the results of previous expressions
is lost when combining standard and non-standard expressions.

Here is an example of this

``` r
iris <- as_tbl(iris)

iris |> 
    f_reframe(
        x = Sepal.Length + Sepal.Width, # Optimised
        y = mean(sum(x)),  # Not currently optimised
        .by = Species
    )
#> Expressions will be evaluated in separate masks
#> Normal exprs: y
#> Optimised exprs: x
#> 
#> To always evaluate everything in the same mask run
#> `fastplyr::fastplyr_disable_optimisations()`
#> It is advised to run these exprs in separate e.g.
#> `f_mutate/f_reframe/f_summarise` statements
#> Run `fastplyr::fastplyr_disable_informative_msgs()` to disable this and other
#> informative messages
#> Error: object 'x' not found
```

To get around this, simply call `f_reframe()` again or `f_mutate()`

``` r
iris |> 
  f_reframe(x = Sepal.Length + Sepal.Width, .by = Species) |> 
  f_mutate(y = mean(sum(x)), .by = Species)
#> # A tibble: 150 × 3
#>   Species     x     y
#>   <fct>   <dbl> <dbl>
#> 1 setosa    8.6  422.
#> 2 setosa    7.9  422.
#> 3 setosa    7.9  422.
#> 4 setosa    7.7  422.
#> 5 setosa    8.6  422.
#> # ℹ 145 more rows
```

## tidytable vs fastplyr

Let’s run some more benchmarks for fun, this time including tidytable
which fastplyr is very similar to as it also uses a tidy frontend but a
data.table backend

### 10 million rows

``` r
n_rows <- 10^7
n_groups <- 10^6

tbl <- new_tbl(x = rnorm(n_rows))
tbl <- tbl |> 
    mutate(y = as.character(round(x, 6)),
           g = sample.int(n_groups, n_rows, TRUE))
tbl
#> # A tibble: 10,000,000 × 3
#>        x y              g
#>    <dbl> <chr>      <int>
#> 1  1.29  1.285351  433366
#> 2 -1.61  -1.613842 887462
#> 3 -0.787 -0.787209 550879
#> 4 -0.490 -0.489809 875660
#> 5  0.393 0.393453  550619
#> # ℹ 9,999,995 more rows
```

### slice benchmark

For this we will be using the `.by` argument from each package. Because
fastplyr still sorts the groups by default here we will set an internal
option to use the alternative grouping algorithm that sorts groups by
order of first appearance. This will likely be revisited at some point.

To read about the differences, see `?collapse::GRP`.

``` r
library(tidytable)
#> Warning: tidytable was loaded after dplyr.
#> This can lead to most dplyr functions being overwritten by tidytable functions.
#> Warning: tidytable was loaded after tidyr.
#> This can lead to most tidyr functions being overwritten by tidytable functions.
#> 
#> Attaching package: 'tidytable'
#> The following objects are masked from 'package:fastplyr':
#> 
#>     across, crossing, desc, n, nesting, pick
#> The following objects are masked from 'package:dplyr':
#> 
#>     across, add_count, add_tally, anti_join, arrange, between,
#>     bind_cols, bind_rows, c_across, case_match, case_when, coalesce,
#>     consecutive_id, count, cross_join, cume_dist, cur_column, cur_data,
#>     cur_group_id, cur_group_rows, dense_rank, desc, distinct, filter,
#>     first, full_join, group_by, group_cols, group_split, group_vars,
#>     if_all, if_any, if_else, inner_join, is_grouped_df, lag, last,
#>     lead, left_join, min_rank, mutate, n, n_distinct, na_if, nest_by,
#>     nest_join, nth, percent_rank, pick, pull, recode, reframe,
#>     relocate, rename, rename_with, right_join, row_number, rowwise,
#>     select, semi_join, slice, slice_head, slice_max, slice_min,
#>     slice_sample, slice_tail, summarise, summarize, tally, top_n,
#>     transmute, tribble, ungroup
#> The following objects are masked from 'package:purrr':
#> 
#>     map, map_chr, map_dbl, map_df, map_dfc, map_dfr, map_int, map_lgl,
#>     map_vec, map2, map2_chr, map2_dbl, map2_df, map2_dfc, map2_dfr,
#>     map2_int, map2_lgl, map2_vec, pmap, pmap_chr, pmap_dbl, pmap_df,
#>     pmap_dfc, pmap_dfr, pmap_int, pmap_lgl, pmap_vec, walk
#> The following objects are masked from 'package:tidyr':
#> 
#>     complete, crossing, drop_na, expand, expand_grid, extract, fill,
#>     nest, nesting, pivot_longer, pivot_wider, replace_na, separate,
#>     separate_longer_delim, separate_rows, separate_wider_delim,
#>     separate_wider_regex, tribble, uncount, unite, unnest,
#>     unnest_longer, unnest_wider
#> The following objects are masked from 'package:tibble':
#> 
#>     enframe, tribble
#> The following objects are masked from 'package:stats':
#> 
#>     dt, filter, lag
#> The following object is masked from 'package:base':
#> 
#>     %in%

tidy_tbl <- as_tidytable(tbl)

# Setting an internal option to set all grouping to use the non-sorted type
options(.fastplyr.order.groups = FALSE)
tidytable::setDTthreads(1) # Single-threaded for fair comparison

mark(
  fastplyr_slice = tbl |> 
  f_slice(3:5, .by = g),
  tidytable_slice = tidy_tbl |> 
    slice(3:5, .by = g),
  check = FALSE,
  min_iterations = 3
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_slice  561.62ms 564.04ms     1.60      136MB    0.535
#> 2 tidytable_slice    4.78s    4.98s     0.201     188MB    1.34
```

### slice_head & slice_tail

``` r
mark(
  fastplyr_slice_head = tbl |> 
  f_slice_head(n = 3, .by = g),
  tidytable_slice_head = tidy_tbl |> 
    slice_head(n = 3, .by = g),
  fastplyr_slice_tail = tbl |> 
  f_slice_tail(n = 3, .by = g),
  tidytable_slice_tail = tidy_tbl |> 
    slice_tail(n = 3, .by = g),
  check = FALSE,
  min_iterations = 3
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 4 × 6
#>   expression                min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>           <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_slice_head  535.18ms 545.87ms     1.79      187MB    0    
#> 2 tidytable_slice_head    1.57s     1.6s     0.612     187MB    0.816
#> 3 fastplyr_slice_tail  563.94ms 638.49ms     1.50      191MB    0.499
#> 4 tidytable_slice_tail    2.75s    2.81s     0.359     187MB    1.20
```

### summarise benchmark

Here we’ll calculate the mean of x by each group of g

Both tidytable and fastplyr have optimisations for `mean()` when it
involves groups. tidytable internally uses data.table’s ‘gforce’ mean
function. This is basically a dedicated C function to calculate means
for many groups.

``` r
mark(
  fastplyr_sumarise = tbl |> 
  f_summarise(mean = mean(x), .by = g),
  tidytable_sumarise = tidy_tbl |> 
  summarise(mean = mean(x), .by = g, .sort = FALSE),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_sumarise     214ms    225ms      4.33    57.2MB    0    
#> 2 tidytable_sumarise    573ms    588ms      1.70   305.3MB    0.851
```

Benchmarking more statistical functions

``` r
mark(
  fastplyr_sumarise2 = tbl |> 
  f_summarise(n = dplyr::n(), mean = mean(x), min = min(x), max = max(x), .by = g),
  tidytable_sumarise2 = tidy_tbl |> 
  summarise(n = n(), mean = mean(x), min = min(x), max = max(x), 
            .by = g, .sort = FALSE),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_sumarise2     321ms    341ms      2.93    72.5MB    1.47 
#> 2 tidytable_sumarise2    712ms    745ms      1.34   320.6MB    0.671
```

### count benchmark

``` r
mark(
  fastplyr_count = tbl |> 
    f_count(y, g),
  tidytable_count = tidy_tbl |> 
    count(y, g),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_count  534.64ms 552.74ms     1.81      229MB    0.905
#> 2 tidytable_count    2.55s    2.55s     0.392     496MB    0.196
```

It’s clear both fastplyr and tidytable are fast and each have their
strengths and weaknesses.
