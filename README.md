
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
#> 1 fastplyr_distinct_sort   10.6ms   11.5ms      84.5    2.95MB     4.33
#> 2 dplyr_distinct_sort      23.7ms   24.3ms      40.4   11.38MB     7.13
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
#> 1 fastplyr_summarise   4.03ms   4.74ms    202.      1.89MB     3.97
#> 2 dplyr_summarise    719.47ms 719.47ms      1.39    9.59MB    23.6
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
#> 1 fastplyr_slice  24.01ms   28.3ms    32.4      21.4MB     11.5
#> 2 dplyr_slice       3.42s    3.42s     0.292    26.6MB     14.3
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
#> 1 fastplyr_group_id   3.25ms    3.8ms    245.      1.46MB     3.99
#> 2 dplyr_group_id    300.54ms  307.8ms      3.25    3.24MB    14.6
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
#> 1 fastplyr_expand  109.6ms  136.6ms    7.37      41.8MB     1.84
#> 2 tidyr_expand       26.5s    26.5s    0.0378   183.5MB     2.91
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
#> 1 fastplyr_duplicates   18.4ms   19.9ms      49.7    45.1MB    49.7 
#> 2 dplyr_duplicates      72.7ms   74.2ms      13.5    59.5MB     2.24
```
