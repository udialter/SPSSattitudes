Set-Up
======

``` r
library(tidyverse)
library(car)
library(psych)
library(mirt) # for multiple IRT
library(GPArotation) # for Promax rotation
library(MBESS) # for reliability CI
library(REdaS) # for assumptions
library(faoutlier) # for efa outliers
library(mice) # for imputations
```

``` r
# Load raw data
full.data <- readxl::read_xlsx("spssdata.xlsx", col_names = TRUE)
# Remove empty rows
full.data <- full.data[1:181, ]

#' __Selecting only SPSS-related columns__
# Do not select StudentIDE col bc you do not want it included with EFA
# Num of rows corresponds to StudentIDE
spss.data <- full.data %>% 
  select(SPSS1E,
         SPSS2E,
         SPSS3E,
         SPSS4E,
         SPSS5E,
         SPSS6E,
         SPSS7E,
         SPSS8E,
         SPSS9E,
         SPSS10E)

#' __Reverse code SPSS items 2, 3, 10__
spss.data$SPSS2E <- car::recode(spss.data$SPSS2E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS3E <- car::recode(spss.data$SPSS3E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS10E <- car::recode(spss.data$SPSS10E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
```