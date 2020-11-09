Iteration and listcols
================
Megan Marziali

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.034845 -0.459888 -0.036290 -0.008707  0.464279  2.442556

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## for loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.705100 2.953893 3.244996 1.841602 4.065483 1.174881 3.570543 4.061815
    ##  [9] 3.204723 4.592685 4.247763 2.264246 2.318728 2.187461 2.629042 2.400789
    ## [17] 3.712335 2.076020 3.769408 1.611204
    ## 
    ## $b
    ##  [1] -4.35432347  3.48503232  6.37456054 -2.97199513  4.23225125  3.54452309
    ##  [7]  1.17651749  2.17689717  2.02807187 -1.11623536  4.21798292 -0.06936006
    ## [13]  0.48663642 -0.89885559 -1.61415260 -5.23251406 -3.20460107  6.36424056
    ## [19] -2.83656042  3.38593449 -6.61474093 -4.34685292 -6.02837778  1.10694507
    ## [25] 10.50871073 -0.50036135 -0.46408076  6.08108654  3.48740818  4.83105606
    ## 
    ## $c
    ##  [1]  9.751002 10.009851  9.736182  9.830905  9.827760 10.167609 10.058244
    ##  [8] 10.320663  9.985739 10.408687  9.905451  9.559720  9.799914  9.892683
    ## [15] 10.286569  9.851536 10.004190  9.576620  9.914276  9.972562 10.027438
    ## [22] 10.014573 10.101292 10.066979  9.950169 10.469438 10.309562 10.003303
    ## [29]  9.776191 10.258215  9.921457  9.917889 10.114002  9.789260 10.264132
    ## [36] 10.038179 10.136499  9.934334  9.943778 10.184134
    ## 
    ## $d
    ##  [1] -1.905971 -4.047220 -2.953331 -4.114615 -2.837094 -3.642651 -3.792838
    ##  [8] -3.244042 -3.579079 -1.890241 -3.568043 -3.680766 -1.715225 -3.657630
    ## [15] -3.417265 -3.688596 -2.337753 -2.126347 -3.615791 -3.015493

Pause and get my old function …

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.93 0.960

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.774  4.18

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.208

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.14 0.758

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

What if you want a different function .. ?

``` r
output = map(list_norm, median)
output = map(list_norm, IQR)
```

Can basically map any function you want across the input list.
