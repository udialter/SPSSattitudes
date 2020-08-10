SPSS Attitudes Measure
================
Carmen Dang
August 9th, 2020

Load packages

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.0.2

``` r
library(haven)
```

    ## Warning: package 'haven' was built under R version 4.0.2

``` r
library(tidyverse)
```

    ## -- Attaching packages -------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v dplyr   1.0.0
    ## v tibble  3.0.1     v stringr 1.4.0
    ## v tidyr   1.1.0     v forcats 0.5.0
    ## v purrr   0.3.4

    ## -- Conflicts ----------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lavaan)
```

    ## Warning: package 'lavaan' was built under R version 4.0.2

    ## This is lavaan 0.6-6

    ## lavaan is BETA software! Please report any bugs.

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.2

    ## 
    ## Attaching package: 'psych'

    ## The following object is masked from 'package:lavaan':
    ## 
    ##     cor2cov

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(mirt)
```

    ## Warning: package 'mirt' was built under R version 4.0.2

    ## Loading required package: stats4

    ## Loading required package: lattice

``` r
library(rsample)
```

    ## Warning: package 'rsample' was built under R version 4.0.2

Load and clean data

Descriptive statistics

Split into EFA/CFA sets

Polychoric correlation

MAP / Parallel Analysis

EFAs
