
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
#>    origin dest 
#>    <chr>  <chr>
#>  1 EWR    IAH  
#>  2 LGA    IAH  
#>  3 JFK    MIA  
#>  4 JFK    BQN  
#>  5 LGA    ATL  
#>  6 EWR    ORD  
#>  7 EWR    FLL  
#>  8 LGA    IAD  
#>  9 JFK    MCO  
#> 10 LGA    ORD  
#> # ℹ 214 more rows
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
#> 1 fastplyr_distinct_sort     12ms   14.2ms      70.6    2.95MB     2.08
#> 2 dplyr_distinct_sort      24.2ms   26.1ms      38.0   11.38MB    10.1
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
#>    month     n
#>    <int> <int>
#>  1     1 27004
#>  2     2 24951
#>  3     3 28834
#>  4     4 28330
#>  5     5 28796
#>  6     6 28243
#>  7     7 29425
#>  8     8 29327
#>  9     9 27574
#> 10    10 28889
#> 11    11 27268
#> 12    12 28135
```

``` r

# Group data is sorted by order-of-first appearance
flights |> 
  f_group_by(month, order = FALSE) |> 
  f_count()
#> # A tibble: 12 × 2
#> # Groups:   month [12]
#>    month     n
#>    <int> <int>
#>  1     1 27004
#>  2    10 28889
#>  3    11 27268
#>  4    12 28135
#>  5     2 24951
#>  6     3 28834
#>  7     4 28330
#>  8     5 28796
#>  9     6 28243
#> 10     7 29425
#> 11     8 29327
#> 12     9 27574
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
#>    month     n
#>    <int> <int>
#>  1     1 27004
#>  2     2 24951
#>  3     3 28834
#>  4     4 28330
#>  5     5 28796
#>  6     6 28243
#>  7     7 29425
#>  8     8 29327
#>  9     9 27574
#> 10    10 28889
#> 11    11 27268
#> 12    12 28135
```

``` r

### With dplyr::group_by

flights |> 
  group_by(month) |> 
  f_count()
#> # A tibble: 12 × 2
#> # Groups:   month [12]
#>    month     n
#>    <int> <int>
#>  1     1 27004
#>  2     2 24951
#>  3     3 28834
#>  4     4 28330
#>  5     5 28796
#>  6     6 28243
#>  7     7 29425
#>  8     8 29327
#>  9     9 27574
#> 10    10 28889
#> 11    11 27268
#> 12    12 28135
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
#>    carrier tailnum origin dest      n mean_dep_delay
#>    <chr>   <chr>   <chr>  <chr> <int>          <dbl>
#>  1 9E      N146PQ  JFK    ATL       8           9.62
#>  2 9E      N153PQ  JFK    ATL       5          -0.4 
#>  3 9E      N161PQ  JFK    ATL       3          -2   
#>  4 9E      N162PQ  EWR    DTW       1         160   
#>  5 9E      N162PQ  JFK    ATL       1          -6   
#>  6 9E      N170PQ  EWR    ATL       3          -5.67
#>  7 9E      N170PQ  JFK    ATL       4          -0.75
#>  8 9E      N176PQ  EWR    DTW       2           8.5 
#>  9 9E      N176PQ  JFK    ATL       5          -3.8 
#> 10 9E      N181PQ  EWR    DTW       2          -7.5 
#> # ℹ 52,797 more rows
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
#> 1 fastplyr_summarise   4.37ms   5.89ms    166.      1.89MB     1.98
#> 2 dplyr_summarise    723.96ms 723.96ms      1.38    9.57MB    23.5
```

### slice

`f_slice` and other `f_slice_` functions are very fast for many groups.

``` r
grouped_flights |> 
  f_slice(1)
#> # A tibble: 52,807 × 19
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1     7      614            615        -1      812            855
#>  2  2013     1     8      612            615        -3      901            855
#>  3  2013     1     9      615            615         0       NA            855
#>  4  2013     1    25     1530           1250       160     1714           1449
#>  5  2013     2    24      609            615        -6      835            855
#>  6  2013     5     1     1140           1145        -5     1339           1403
#>  7  2013     1    10      613            615        -2      840            855
#>  8  2013     2     1     1301           1250        11     1458           1449
#>  9  2013     1    17      612            615        -3      906            855
#> 10  2013     1    11     1244           1250        -6     1459           1449
#> # ℹ 52,797 more rows
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

``` r

grouped_flights |>
  f_slice_head(3)
#> # A tibble: 125,770 × 19
#> # Groups:   carrier, tailnum, origin, dest [52,807]
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1     7      614            615        -1      812            855
#>  2  2013     1    13      612            615        -3      853            855
#>  3  2013     2     3      617            615         2      902            855
#>  4  2013     1     8      612            615        -3      901            855
#>  5  2013     1    22      614            615        -1      857            855
#>  6  2013     1    27      617            615         2      831            855
#>  7  2013     1     9      615            615         0       NA            855
#>  8  2013     1    12      613            615        -2      839            855
#>  9  2013     2     5      611            615        -4      847            855
#> 10  2013     1    25     1530           1250       160     1714           1449
#> # ℹ 125,760 more rows
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
#> 1 fastplyr_slice   27.7ms   32.1ms    30.8      21.4MB     11.6
#> 2 dplyr_slice       3.65s    3.65s     0.274    26.6MB     13.7
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
#>    carrier tailnum origin dest  group_id
#>    <chr>   <chr>   <chr>  <chr>    <int>
#>  1 UA      N14228  EWR    IAH      35951
#>  2 UA      N24211  LGA    IAH      36937
#>  3 AA      N619AA  JFK    MIA       8489
#>  4 B6      N804JB  JFK    BQN      15462
#>  5 DL      N668DN  LGA    ATL      20325
#>  6 UA      N39463  EWR    ORD      39386
#>  7 B6      N516JB  EWR    FLL      10606
#>  8 EV      N829AS  LGA    IAD      33062
#>  9 B6      N593JB  JFK    MCO      12483
#> 10 AA      N3ALAA  LGA    ORD       4468
#> # ℹ 336,766 more rows
```

Another benchmark

``` r
mark(
  fastplyr_group_id = grouped_flights |> 
  add_group_id() |> 
  f_select(group_id),
  dplyr_group_id = grouped_flights |> 
  mutate(group_id = cur_group_id()) |> 
  select(group_id)
)
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: `carrier`, `tailnum`, `origin`, `dest`
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: 'carrier', 'tailnum', 'origin', 'dest'
#> Adding missing grouping variables: `carrier`, `tailnum`, `origin`, `dest`
#> Adding missing grouping variables: `carrier`, `tailnum`, `origin`, `dest`
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 × 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 fastplyr_group_id   3.11ms   3.94ms    240.      1.29MB     3.97
#> 2 dplyr_group_id    319.88ms    320ms      3.13    3.77MB    12.5
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
#> 1 fastplyr_expand  134.6ms  143.2ms    6.71      41.8MB     6.71
#> 2 tidyr_expand       25.5s    25.5s    0.0392   183.5MB     4.32
```

### duplicate rows

Finding duplicate rows is a very common dataset operation and there is a
dedicated function `f_duplicates()` to do exactly this.

``` r
flights |> 
  f_duplicates(time_hour)
#> # A tibble: 329,840 × 1
#>    time_hour          
#>    <dttm>             
#>  1 2013-01-01 05:00:00
#>  2 2013-01-01 05:00:00
#>  3 2013-01-01 05:00:00
#>  4 2013-01-01 05:00:00
#>  5 2013-01-01 06:00:00
#>  6 2013-01-01 06:00:00
#>  7 2013-01-01 06:00:00
#>  8 2013-01-01 06:00:00
#>  9 2013-01-01 06:00:00
#> 10 2013-01-01 06:00:00
#> # ℹ 329,830 more rows
```

Benchmark against a common dplyr stratefy for finding duplicates

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
#> 1 fastplyr_duplicates   23.4ms   27.7ms      23.1   180.4MB     19.2
#> 2 dplyr_duplicates      75.3ms   83.3ms      12.1    60.7MB     12.1
```
