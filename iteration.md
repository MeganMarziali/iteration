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
    ## -3.424904 -0.609248  0.009429 -0.045475  0.587345  2.320980

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
    ##  [1] 2.068495 2.081721 2.323376 2.557080 1.839517 3.718728 3.106576 5.800325
    ##  [9] 2.381749 3.836936 2.769029 2.383077 2.518164 2.579632 1.100594 2.159489
    ## [17] 2.786475 2.536826 3.403421 3.303074
    ## 
    ## $b
    ##  [1]  -3.41496374   8.06487146  -1.98616756  -6.48127322  -7.39065326
    ##  [6]   5.89324255   0.58961098  -3.73250876   3.71993922  -9.12220007
    ## [11]   2.11230850  -0.93547930  -2.87800377   2.97183521  -3.00195464
    ## [16]   5.87112189  -3.71042737  -4.59339940  -3.30121077 -16.72235295
    ## [21]  -2.08385080   0.26572743   2.38124222   6.16784869  -0.11096528
    ## [26]  -8.14406987  -3.52829609  -0.01849744  -2.75997725   0.11608518
    ## 
    ## $c
    ##  [1] 10.321696 10.102581 10.199233 10.143837  9.999588  9.580560 10.249432
    ##  [8] 10.264590  9.620108  9.760437  9.915551 10.226532  9.825513  9.919895
    ## [15] 10.053423  9.789030  9.746024  9.913247  9.857931  9.649027 10.048071
    ## [22] 10.124715 10.007186  9.720149 10.191606  9.994569  9.954535 10.070848
    ## [29]  9.893943  9.874654  9.936002  9.852661  9.750617  9.513049  9.646893
    ## [36] 10.245359 10.096524 10.066049  9.904079 10.031814
    ## 
    ## $d
    ##  [1] -3.2263204 -2.9152519 -1.9524100 -2.8827865 -3.0594557 -2.9519267
    ##  [7] -2.3205218 -5.7444096 -2.6827872 -2.0380899 -3.2281498 -2.5362816
    ## [13] -2.4279725 -2.9547127 -1.6132879 -3.2434904 -1.8468244 -0.8165462
    ## [19] -4.6366631 -3.9019072

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
    ## 1  2.76 0.965

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.53  5.18

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.206

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.85  1.07

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

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.068495 2.081721 2.323376 2.557080 1.839517 3.718728 3.106576 5.800325
    ##  [9] 2.381749 3.836936 2.769029 2.383077 2.518164 2.579632 1.100594 2.159489
    ## [17] 2.786475 2.536826 3.403421 3.303074
    ## 
    ## $b
    ##  [1]  -3.41496374   8.06487146  -1.98616756  -6.48127322  -7.39065326
    ##  [6]   5.89324255   0.58961098  -3.73250876   3.71993922  -9.12220007
    ## [11]   2.11230850  -0.93547930  -2.87800377   2.97183521  -3.00195464
    ## [16]   5.87112189  -3.71042737  -4.59339940  -3.30121077 -16.72235295
    ## [21]  -2.08385080   0.26572743   2.38124222   6.16784869  -0.11096528
    ## [26]  -8.14406987  -3.52829609  -0.01849744  -2.75997725   0.11608518
    ## 
    ## $c
    ##  [1] 10.321696 10.102581 10.199233 10.143837  9.999588  9.580560 10.249432
    ##  [8] 10.264590  9.620108  9.760437  9.915551 10.226532  9.825513  9.919895
    ## [15] 10.053423  9.789030  9.746024  9.913247  9.857931  9.649027 10.048071
    ## [22] 10.124715 10.007186  9.720149 10.191606  9.994569  9.954535 10.070848
    ## [29]  9.893943  9.874654  9.936002  9.852661  9.750617  9.513049  9.646893
    ## [36] 10.245359 10.096524 10.066049  9.904079 10.031814
    ## 
    ## $d
    ##  [1] -3.2263204 -2.9152519 -1.9524100 -2.8827865 -3.0594557 -2.9519267
    ##  [7] -2.3205218 -5.7444096 -2.6827872 -2.0380899 -3.2281498 -2.5362816
    ## [13] -2.4279725 -2.9547127 -1.6132879 -3.2434904 -1.8468244 -0.8165462
    ## [19] -4.6366631 -3.9019072

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.76 0.965

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.53  5.18

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.76 0.965
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.53  5.18
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.95 0.206
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.85  1.07

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map(samp, median)
  )
```
