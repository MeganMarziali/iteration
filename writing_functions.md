Writing Functions
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

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd  = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -2.75853743 -0.69369446  1.81999422  1.12915562  0.79333147 -1.31557885
    ##  [7]  0.37937918  0.66394041  0.88479528 -0.26614012  0.56122130  0.23431676
    ## [13]  0.15304189  0.86022748 -0.65165968  0.09557443  0.08012992  0.44837519
    ## [19] -1.18943235 -0.41900010  0.01980750 -0.87838014  0.51402332 -0.40332809
    ## [25]  0.97504479 -1.08412783  0.55716515  0.69362545 -2.07100772  0.86773742

I want a function to compute z-scores.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -2.75853743 -0.69369446  1.81999422  1.12915562  0.79333147 -1.31557885
    ##  [7]  0.37937918  0.66394041  0.88479528 -0.26614012  0.56122130  0.23431676
    ## [13]  0.15304189  0.86022748 -0.65165968  0.09557443  0.08012992  0.44837519
    ## [19] -1.18943235 -0.41900010  0.01980750 -0.87838014  0.51402332 -0.40332809
    ## [25]  0.97504479 -1.08412783  0.55716515  0.69362545 -2.07100772  0.86773742

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric
