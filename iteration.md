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
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.55651 -0.68655 -0.03123 -0.04707  0.57715  2.02425

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
    ##  [1] 3.997525 3.941368 2.586511 3.603443 3.406334 1.711781 1.476825 3.183125
    ##  [9] 2.472902 4.819950 3.373423 3.245905 4.200842 3.057002 4.505553 4.983309
    ## [17] 3.073066 2.806187 1.981152 3.055046
    ## 
    ## $b
    ##  [1] -4.48786570  1.85891901 -2.66471629  0.87753077 -4.08957256 -0.48819161
    ##  [7] -0.04051749 -0.99040615  1.54205227 -5.44436102  0.78713381 -1.18077853
    ## [13]  5.04060935  5.34852197 -2.72244289  3.16770476  2.46559793 -0.59237640
    ## [19] -1.32814252  2.00019360  8.86752396  2.22429909  9.26360043  3.13619029
    ## [25] -0.83663889  2.54685403 -0.69472979  0.72945988  9.31039039 -6.62203846
    ## 
    ## $c
    ##  [1] 10.067213  9.785443  9.965134  9.958709 10.351121 10.050619  9.569615
    ##  [8] 10.122066 10.474887  9.764752  9.942713 10.071488  9.884316 10.387891
    ## [15]  9.939192 10.574094 10.083095 10.419489 10.014230  9.838718 10.115364
    ## [22]  9.820717 10.052162  9.489321  9.754431  9.912137 10.084424  9.993477
    ## [29]  9.901537 10.172783 10.316279 10.015699 10.154091  9.990646  9.913123
    ## [36]  9.705200  9.938727 10.152691  9.990769 10.092178
    ## 
    ## $d
    ##  [1] -3.000387 -3.515758 -1.926944 -2.210338 -1.940970 -2.611291 -3.599803
    ##  [8] -3.546819 -2.180240 -3.322678 -2.012427 -4.187843 -1.694513 -3.295285
    ## [15] -4.006519 -4.986818 -3.109933 -4.258116 -3.680216 -2.776188

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
    ## 1  3.27 0.963

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.899  3.99

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.228

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.09 0.913

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

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```
