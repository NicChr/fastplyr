
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastplyr

<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/fastplyr/actions/workflows/R-CMD-check.yaml)

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
```

``` r
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
```

``` r
library(nycflights13)
library(bench)
```

While the syntax and user-interface of fastplyr aligns very closely with
dplyr most of the time, there can be a few key differences.

## Differences between fastplyr and dplyr

<table style="width:100%;">
<colgroup>
<col style="width: 10%" />
<col style="width: 43%" />
<col style="width: 46%" />
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
<code>f_group_by(order = F)</code></td>
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
  f_distinct(origin, dest, tailnum, sort = TRUE),
  dplyr_distinct_sort = flights |> 
    distinct(origin, dest, tailnum) |> 
    arrange_all()
)
#> # A tibble: 2 × 6
#>   expression                  min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>             <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_distinct_sort     11ms   12.1ms      80.9    2.95MB     2.08
#> 2 dplyr_distinct_sort      24.1ms   25.4ms      38.0   11.38MB     7.61
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
```

``` r

# Group data is sorted by order-of-first appearance
flights |> 
  f_group_by(month, order = FALSE) |> 
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
#> 1 fastplyr_summarise   4.14ms   5.01ms    178.      2.09MB     3.99
#> 2 dplyr_summarise    691.81ms 691.81ms      1.45    9.59MB     8.67
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
```

``` r

# inner join

left |> 
  f_inner_join(right)
#> # A tibble: 243,083 × 14
#>   origin dest  time_hour             sec   min  hour  mday   mon  year  wday
#>   <chr>  <chr> <dttm>              <dbl> <int> <int> <int> <int> <int> <int>
#> 1 EWR    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 2 LGA    IAH   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 3 JFK    MIA   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 4 JFK    BQN   2013-01-01 05:00:00     0     0     5     1     0   113     2
#> 5 LGA    ATL   2013-01-01 06:00:00     0     0     6     1     0   113     2
#> # ℹ 243,078 more rows
#> # ℹ 4 more variables: yday <int>, isdst <int>, zone <chr>, gmtoff <int>
```

``` r

# Anti join

left |> 
  f_anti_join(right)
#> # A tibble: 93,693 × 3
#>   origin dest  time_hour          
#>   <chr>  <chr> <dttm>             
#> 1 EWR    MCO   2013-01-01 08:00:00
#> 2 JFK    MIA   2013-01-01 08:00:00
#> 3 LGA    MSP   2013-01-01 08:00:00
#> 4 JFK    RDU   2013-01-01 08:00:00
#> 5 EWR    BOS   2013-01-01 08:00:00
#> # ℹ 93,688 more rows
```

``` r

# Semi join

left |> 
  f_semi_join(right)
#> # A tibble: 243,083 × 3
#>   origin dest  time_hour          
#>   <chr>  <chr> <dttm>             
#> 1 EWR    IAH   2013-01-01 05:00:00
#> 2 LGA    IAH   2013-01-01 05:00:00
#> 3 JFK    MIA   2013-01-01 05:00:00
#> 4 JFK    BQN   2013-01-01 05:00:00
#> 5 LGA    ATL   2013-01-01 06:00:00
#> # ℹ 243,078 more rows
```

``` r

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
#> 1 fastplyr_left_join     11ms   12.5ms      79.3    19.3MB     39.7
#> 2 dplyr_left_join        30ms   35.1ms      25.4      45MB     76.3
```

``` r
mark(
  fastplyr_inner_join = f_inner_join(left, right, by = "time_hour"),
  dplyr_inner_join = inner_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_inner_join   6.58ms   10.3ms      99.7    22.1MB     36.9
#> 2 dplyr_inner_join      25.4ms     27ms      35.2    37.8MB     39.6
```

``` r
mark(
  fastplyr_anti_join = f_anti_join(left, right, by = "time_hour"),
  dplyr_anti_join = anti_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_anti_join   2.34ms    3.4ms     262.     3.79MB     16.2
#> 2 dplyr_anti_join     16.18ms   19.2ms      50.7   21.82MB     20.9
```

``` r
mark(
  fastplyr_semi_join = f_semi_join(left, right, by = "time_hour"),
  dplyr_semi_join = semi_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_semi_join    3.6ms   5.54ms     189.     7.78MB     22.4
#> 2 dplyr_semi_join      15.9ms  17.72ms      56.3   21.92MB     34.6
```

``` r
mark(
  fastplyr_full_join = f_full_join(left, right, by = "time_hour"),
  dplyr_full_join = full_join(left, right, by = "time_hour")
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_full_join   10.1ms   12.6ms      80.0    20.6MB     40.0
#> 2 dplyr_full_join      31.3ms   32.4ms      30.4    44.6MB     76.0
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
```

``` r

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
#> 1 fastplyr_slice  23.79ms  27.98ms    32.9      21.4MB     13.6
#> 2 dplyr_slice       3.42s    3.42s     0.292    26.6MB     10.5
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
#> 1 fastplyr_group_id   3.15ms   3.78ms    248.      1.46MB     3.97
#> 2 dplyr_group_id    312.98ms 323.19ms      3.09    3.24MB    10.8
```

### expand

Based closely on `tidyr::expand`, `f_expand()` can cross joins multiple
vectors and data frames.

``` r
mark(
  fastplyr_expand = grouped_flights |> 
  f_expand(month = 1:12),
  tidyr_expand = grouped_flights |> 
  expand(month = 1:12),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_expand  111.5ms  132.3ms    7.68      41.8MB     5.76
#> 2 tidyr_expand       26.3s    26.3s    0.0380   183.5MB     2.55
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
#> 1 fastplyr_duplicates     18ms     27ms      38.0    45.1MB     10.9
#> 2 dplyr_duplicates      64.1ms   70.9ms      13.8    59.5MB     10.3
```

### filter

In the wosrt case scenarios, `f_filter()` is about the same speed as
`filter()` and in the best case is much faster and more efficient. This
is especially true for large data where small subsets of the data are
returned.

``` r
full <- tibble(x = rnorm(5e07))

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
#> 1 fastplyr_filter    597ms    597ms      1.67    1.12GB     1.67
#> 2 dplyr_filter       866ms    866ms      1.16    1.68GB     2.31
```

``` r

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
#> 1 fastplyr_filter    177ms    182ms      5.40     191MB     0   
#> 2 dplyr_filter       486ms    503ms      1.99     763MB     1.99
```

## bind rows and cols

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
#> 1 fastplyr_bind_cols   53.3µs   62.4µs  13917.     46.95KB     4.01
#> 2 dplyr_bind_cols     228.4ms  230.4ms      4.18    1.31MB     4.18
```

``` r

mark(
  fastplyr_bind_rows = f_bind_rows(grouped_flights, grouped_flights),
  dplyr_bind_rows = bind_rows(grouped_flights, grouped_flights)
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_bind_rows   58.5ms   61.9ms     16.1     86.5MB     0   
#> 2 dplyr_bind_rows     225.2ms  232.3ms      4.31   157.6MB     2.15
```

## tidytable vs fastplyr

Let’s run some more benchmarks for fun, this time including tidytable
which fastplyr is very similar to as it also uses a tidy frontend but a
data.table backend

### 10 million rows

``` r
n_rows <- 10^7
n_groups <- 10^6

tbl <- tibble(x = rnorm(n_rows))
tbl <- tbl |> 
    mutate(y = as.character(round(x, 6)),
           g = sample.int(n_groups, n_rows, TRUE))
tbl
#> # A tibble: 10,000,000 × 3
#>         x y              g
#>     <dbl> <chr>      <int>
#> 1  1.33   1.325541  227215
#> 2 -0.625  -0.624903 467817
#> 3  0.824  0.823999  926876
#> 4  0.0622 0.062231  464841
#> 5 -0.0443 -0.044286 776911
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
```

``` r

tidy_tbl <- as_tidytable(tbl)

# Setting an internal option to set all grouping to use the non-sorted type
options(.fastplyr.groups.sort = FALSE)

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
#> 1 fastplyr_slice  877.85ms    1.03s     1.02      140MB    0.679
#> 2 tidytable_slice    6.78s    6.78s     0.145     176MB    2.27
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
#> 1 fastplyr_slice_head  859.35ms    1.04s     0.983     191MB    0.328
#> 2 tidytable_slice_head    1.84s    1.99s     0.445     175MB    1.78 
#> 3 fastplyr_slice_tail  921.57ms 938.72ms     1.06      194MB    0    
#> 4 tidytable_slice_tail    3.66s    3.76s     0.267     175MB    2.23
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
  summarise(mean = mean(x), .by = g),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_sumarise     329ms    335ms      2.98    57.2MB     0   
#> 2 tidytable_sumarise    241ms    252ms      3.97   290.2MB     1.99
```

Benchmarking more statistical functions

``` r
mark(
  fastplyr_sumarise2 = tbl |> 
  f_summarise(n = n(), mean = mean(x), min = min(x), max = max(x), .by = g),
  tidytable_sumarise2 = tidy_tbl |> 
  summarise(n = n(), mean = mean(x), min = min(x), max = max(x), .by = g),
  check = FALSE,
  min_iterations = 3
)
#> # A tibble: 2 × 6
#>   expression               min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_sumarise2     406ms    431ms      2.31    72.5MB     0   
#> 2 tidytable_sumarise2    432ms    441ms      2.27   305.4MB     1.13
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
#> 1 fastplyr_count  337.79ms 348.55ms     2.87      229MB    1.43 
#> 2 tidytable_count    3.92s    3.95s     0.253     496MB    0.127
```

It’s clear both fastplyr and tidytable are fast and each have their
strengths and weaknesses.
