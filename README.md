
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastplyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/fastplyr)](https://CRAN.R-project.org/package=fastplyr)

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
#> Warning: package 'dplyr' was built under R version 4.4.1
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
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

<table style="width:100%;">
<colgroup>
<col style="width: 9%" />
<col style="width: 40%" />
<col style="width: 49%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th><h3 id="dplyr">dplyr</h3></th>
<th><h3 id="fastplyr-1">fastplyr</h3></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>.by</code></td>
<td>Groups are sorted by order of first appearance always when using
<code>.by</code></td>
<td>Groups are always sorted by default, even when using
<code>.by</code>. One can use the other sorting through
<code>f_group_by(.order = F)</code></td>
</tr>
<tr class="even">
<td>Many groups</td>
<td>Generally slow for data with many groups.</td>
<td>Designed to be fast for data with many groups.</td>
</tr>
<tr class="odd">
<td>Handling of dots (<code>...</code>)</td>
<td>dplyr almost always executes <code>...</code> expressions in a way
that latter expressions depend on previous ones</td>
<td>Some functions like <code>f_summarise</code> and
<code>f_expand</code> execute the expressions in <code>...</code>
independently.</td>
</tr>
<tr class="even">
<td>Duplicate rows</td>
<td>No dedicated function for this, solution using
<code>group_by |&gt; filter(n() &gt; 1)</code> are generally slow for
larger data.</td>
<td>Dedicated function <code>f_duplicates</code> can do this very fast
and with fine control.</td>
</tr>
<tr class="odd">
<td>Unique group IDs</td>
<td>Achieved through <code>mutate(cur_group_id())</code></td>
<td>Dedicated fast function <code>add_group_id()</code></td>
</tr>
<tr class="even">
<td>Row slicing</td>
<td><code>slice()</code> supports data-masked expressions supplied to
<code>...</code></td>
<td>Data-masked expressions not supported in <code>f_slice_</code>
functions. Use <code>f_filter()</code> for this behaviour.</td>
</tr>
<tr class="odd">
<td>Memory usage</td>
<td>High memory usage</td>
<td>Lower usage compared to dplyr</td>
</tr>
<tr class="even">
<td>joins</td>
<td>Accepts different types of joins, e.g. rolling and equality
joins.</td>
<td>Accepts only equality joins of the form <code>x == y</code></td>
</tr>
<tr class="odd">
<td>rowwise</td>
<td><code>rowwise_df</code> accepted and everything sub-setted implictly
using <code>[[</code></td>
<td><code>rowwise_df</code> not accepted, must use
<code>f_rowwise_df</code> which creates a <code>grouped_df</code> with a
row ID col. Implicit <code>[[</code> subsetting does not occur.</td>
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

`f_distinct` has an additional `sort` argument which is much faster than
sorting afterwards.

``` r
mark(
  fastplyr_distinct_sort = flights |> 
  f_distinct(origin, dest, tailnum, .sort = TRUE),
  dplyr_distinct_sort = flights |> 
    distinct(origin, dest, tailnum) |> 
    arrange_all()
)
#> # A tibble: 2 × 6
#>   expression                  min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>             <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_distinct_sort   10.5ms   11.5ms      85.8    2.95MB     4.19
#> 2 dplyr_distinct_sort      22.9ms     24ms      41.9   11.38MB     7.39
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
#> # Groups:   month [12]
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
#> # Groups:   month [12]
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
#> 1 fastplyr_summarise   4.24ms   5.43ms    175.      2.09MB     3.98
#> 2 dplyr_summarise    654.99ms 654.99ms      1.53    9.59MB    10.7
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
#> * <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
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
#> * <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
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
#> 1 fastplyr_left_join   11.6ms     12ms      83.4    19.3MB     72.3
#> 2 dplyr_left_join      32.1ms   32.1ms      31.1      45MB    171.
```

``` r
mark(
  fastplyr_inner_join = f_inner_join(left, right, by = "time_hour"),
  dplyr_inner_join = inner_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_inner_join   5.11ms   9.42ms     111.     22.2MB     60.0
#> 2 dplyr_inner_join     24.42ms  25.93ms      38.5    37.9MB     66.0
```

``` r
mark(
  fastplyr_anti_join = f_anti_join(left, right, by = "time_hour"),
  dplyr_anti_join = anti_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_anti_join   2.29ms   3.19ms     329.     3.76MB     26.6
#> 2 dplyr_anti_join     14.48ms  18.08ms      56.0    21.8MB     23.6
```

``` r
mark(
  fastplyr_semi_join = f_semi_join(left, right, by = "time_hour"),
  dplyr_semi_join = semi_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_semi_join   3.72ms   5.54ms     186.      7.8MB     17.7
#> 2 dplyr_semi_join     16.53ms   19.7ms      51.7    21.9MB     12.9
```

``` r
mark(
  fastplyr_full_join = f_full_join(left, right, by = "time_hour"),
  dplyr_full_join = full_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_full_join   9.54ms   12.4ms      82.9    20.6MB     52.7
#> 2 dplyr_full_join     32.76ms   34.2ms      29.2    44.6MB    161.
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
#> 1 fastplyr_slice  23.63ms  27.97ms    33.0      21.4MB     11.7
#> 2 dplyr_slice       3.32s    3.32s     0.301    26.6MB     11.2
```

### Group IDs

In dplyr to work with group IDs you must use the `mutate()` +
`cur_group_id()` paradigm.

In fastplyr you can just use `add_group_id()` which is blazing fast.

``` r

## Unique ID for each group

grouped_flights |> 
  add_group_id() |> 
  f_select(group_id)
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> # A tibble: 336,776 × 5
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>   carrier tailnum origin dest  group_id
#>   <chr>   <chr>   <chr>  <chr>    <int>
#> 1 UA      N14228  EWR    IAH      35951
#> 2 UA      N24211  LGA    IAH      36937
#> 3 AA      N619AA  JFK    MIA       8489
#> 4 B6      N804JB  JFK    BQN      15462
#> 5 DL      N668DN  LGA    ATL      20325
#> # ℹ 336,771 more rows
```

Another benchmark

``` r
mark(
  fastplyr_group_id = grouped_flights |> 
  add_group_id() |> 
  f_select(all_of(group_vars(grouped_flights)), group_id),
  dplyr_group_id = grouped_flights |> 
  mutate(group_id = cur_group_id()) |> 
  select(all_of(group_vars(grouped_flights)), group_id)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_group_id   3.08ms   3.62ms    252.      1.46MB     3.97
#> 2 dplyr_group_id    276.47ms 277.66ms      3.60    3.24MB     9.00
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
#> 1 fastplyr_expand  20.97ms   23.1ms    23.8      8.86MB     3.96
#> 2 tidyr_expand       3.63s    3.63s     0.276   81.02MB     5.51


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
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_expand   17.9ms   18.7ms     52.7     15.5MB     7.03
#> 2 tidyr_expand     201.8ms  201.8ms      4.95    66.7MB     9.91
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
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_duplicates   22.9ms   27.8ms      28.2    45.1MB     19.4
#> 2 dplyr_duplicates      70.6ms   76.4ms      12.7    59.5MB     12.7
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
#> 1 fastplyr_filter 776.63ms 776.63ms     1.29     1.12GB     1.29
#> 2 dplyr_filter       1.07s    1.07s     0.935    1.68GB     1.87

# Best case scenario - filter results in small subset

mark(
  fastplyr_filter = full |> 
    f_filter(x > 4),
  dplyr_filter = full |> 
    filter(x > 4)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_filter    178ms    178ms      4.59     191MB     0   
#> 2 dplyr_filter       527ms    527ms      1.90     763MB     1.90
```

### bind rows and cols

Binding columns is particular much faster but binding rows is also
sufficiently faster

``` r
mark(
  fastplyr_bind_cols = f_bind_cols(grouped_flights, grouped_flights),
  dplyr_bind_cols = suppressMessages(
    bind_cols(grouped_flights, grouped_flights)
    )
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_bind_cols     56µs   61.6µs  14702.     42.28KB     4.01
#> 2 dplyr_bind_cols       228ms  231.5ms      3.99    1.31MB     3.99

mark(
  fastplyr_bind_rows = f_bind_rows(grouped_flights, grouped_flights),
  dplyr_bind_rows = bind_rows(grouped_flights, grouped_flights)
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_bind_rows   59.5ms   60.6ms     15.8     86.6MB     0   
#> 2 dplyr_bind_rows     231.1ms  231.1ms      4.33   157.6MB     4.33
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
  tidy_quantiles(mpg, .by = cyl)
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
    f_group_by(year, month, day, origin) |> 
  tidy_quantiles(dep_delay, pivot = "long"),
  dplyr_quantiles = flights |> 
     group_by(year, month, day, origin) |> 
    reframe(enframe(quantile(dep_delay, seq(0, 1, 0.25), na.rm = TRUE))),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_quantiles   25.6ms     27ms     36.0     7.17MB     2.00
#> 2 dplyr_quantiles       210ms    225ms      4.18   24.98MB     4.18
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
#>     crossing, desc, nesting
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
#> 1 fastplyr_slice   753.1ms 788.94ms     1.21      133MB    0.403
#> 2 tidytable_slice     6.1s    6.28s     0.159     176MB    1.69
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
#> 1 fastplyr_slice_head  741.79ms 809.05ms     1.27      191MB     0   
#> 2 tidytable_slice_head    1.55s    1.59s     0.617     175MB     1.44
#> 3 fastplyr_slice_tail   797.1ms 797.22ms     1.23      194MB     0   
#> 4 tidytable_slice_tail     3.2s     3.3s     0.300     175MB     1.70
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
#> 1 fastplyr_sumarise     260ms    304ms      3.04    57.2MB     0   
#> 2 tidytable_sumarise    261ms    301ms      3.32   305.4MB     1.66
```

Benchmarking more statistical functions

``` r
mark(
  fastplyr_sumarise2 = tbl |> 
  f_summarise(n = n(), mean = mean(x), min = min(x), max = max(x), .by = g),
  tidytable_sumarise2 = tidy_tbl |> 
  summarise(n = n(), mean = mean(x), min = min(x), max = max(x), 
            .by = g, .sort = FALSE),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_sumarise2     342ms    346ms      2.89    72.5MB     1.44
#> 2 tidytable_sumarise2    426ms    440ms      2.27   320.7MB     1.14
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
#> 1 fastplyr_count  337.88ms 344.23ms     2.91      229MB    1.45 
#> 2 tidytable_count    3.38s    3.38s     0.295     496MB    0.148
```

It’s clear both fastplyr and tidytable are fast and each have their
strengths and weaknesses.
