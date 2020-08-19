SPSS Attitudes Measure Validation
================
Carmen Dang
13/07/2020

    ## Warning: package 'readr' was built under R version 4.0.2

    ## Warning: package 'haven' was built under R version 4.0.2

    ## -- Attaching packages -------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v dplyr   1.0.0
    ## v tibble  3.0.1     v stringr 1.4.0
    ## v tidyr   1.1.0     v forcats 0.5.0
    ## v purrr   0.3.4

    ## -- Conflicts ----------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Warning: package 'lavaan' was built under R version 4.0.2

    ## This is lavaan 0.6-6

    ## lavaan is BETA software! Please report any bugs.

    ## Warning: package 'psych' was built under R version 4.0.2

    ## 
    ## Attaching package: 'psych'

    ## The following object is masked from 'package:lavaan':
    ## 
    ##     cor2cov

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

    ## Warning: package 'mirt' was built under R version 4.0.2

    ## Loading required package: stats4

    ## Loading required package: lattice

    ## Warning: package 'rsample' was built under R version 4.0.2

``` r
# Uploading raw data
full.data <- readxl::read_xlsx("spssdata.xlsx", col_names = TRUE)
# Remove empty rows
full.data <- full.data[1:181, ]

# Selecting only SPSS-related columns
  # Do not select StudentIDE col bc it'd be included with EFA/CFA
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

# Reverse code SPSS items 2, 3, 10
spss.data$SPSS2E <- car::recode(spss.data$SPSS2E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS3E <- car::recode(spss.data$SPSS3E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS10E <- car::recode(spss.data$SPSS10E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
```

# Questionnaire Info

<!-- 1 Always True 2 Mostly True    3 Sometimes True 4 Rarely True 5 Never True -->

<!-- SPSS1E SPSS helped me understand statistics concepts better. -->

<!-- SPSS2E SPSS has not helped me do well in the course. -->

<!-- SPSS3E SPSS has not helped me integrate and apply class concepts. -->

<!-- SPSS4E SPSS has helped me become more comfortable with statistics. -->

<!-- SPSS5E SPSS provided me with a good application experience. -->

<!-- SPSS6E SPSS has been convenient for me to do assignments. -->

<!-- SPSS7E I think the skills I’m learning with SPSS will be useful outside of class. -->

<!-- SPSS8E SPSS helps me get involved in learning class material. -->

<!-- SPSS9E SPSS has helped me gain confidence in the course material. -->

<!-- SPSS10E    SPSS has not helped me make more sense out of the material. -->

# Descriptives

``` r
describe(spss.data)
```

    ##         vars   n mean   sd median trimmed  mad min max range  skew kurtosis
    ## SPSS1E     1 180 3.49 1.04      4    3.55 1.48   1   5     4 -0.44    -0.30
    ## SPSS2E     2 181 3.67 1.11      4    3.76 1.48   1   5     4 -0.58    -0.58
    ## SPSS3E     3 180 3.51 1.11      4    3.58 1.48   1   5     4 -0.53    -0.43
    ## SPSS4E     4 181 3.38 0.99      3    3.39 1.48   1   5     4 -0.30    -0.42
    ## SPSS5E     5 180 3.70 1.05      4    3.79 1.48   1   5     4 -0.52    -0.27
    ## SPSS6E     6 181 3.52 1.22      4    3.64 1.48   1   5     4 -0.51    -0.60
    ## SPSS7E     7 181 3.32 1.18      3    3.34 1.48   1   5     4 -0.13    -0.98
    ## SPSS8E     8 181 3.57 0.98      4    3.63 1.48   1   5     4 -0.40    -0.22
    ## SPSS9E     9 181 3.43 1.03      3    3.46 1.48   1   5     4 -0.25    -0.44
    ## SPSS10E   10 181 3.41 1.13      4    3.46 1.48   1   5     4 -0.33    -0.71
    ##           se
    ## SPSS1E  0.08
    ## SPSS2E  0.08
    ## SPSS3E  0.08
    ## SPSS4E  0.07
    ## SPSS5E  0.08
    ## SPSS6E  0.09
    ## SPSS7E  0.09
    ## SPSS8E  0.07
    ## SPSS9E  0.08
    ## SPSS10E 0.08

``` r
# Contingency table of the counts
table(spss.data$SPSS1E)
```

    ## 
    ##  1  2  3  4  5 
    ##  8 21 55 66 30

``` r
table(spss.data$SPSS2E)
```

    ## 
    ##  1  2  3  4  5 
    ##  6 28 31 70 46

``` r
table(spss.data$SPSS3E)
```

    ## 
    ##  1  2  3  4  5 
    ## 10 24 43 70 33

``` r
table(spss.data$SPSS4E)
```

    ## 
    ##  1  2  3  4  5 
    ##  6 28 59 67 21

``` r
table(spss.data$SPSS5E)
```

    ## 
    ##  1  2  3  4  5 
    ##  6 15 52 61 46

``` r
table(spss.data$SPSS6E)
```

    ## 
    ##  1  2  3  4  5 
    ## 16 17 51 50 47

``` r
table(spss.data$SPSS7E)
```

    ## 
    ##  1  2  3  4  5 
    ## 10 40 48 48 35

``` r
table(spss.data$SPSS8E)
```

    ## 
    ##  1  2  3  4  5 
    ##  5 18 58 68 32

``` r
table(spss.data$SPSS9E)
```

    ## 
    ##  1  2  3  4  5 
    ##  7 24 64 57 29

``` r
table(spss.data$SPSS10E)
```

    ## 
    ##  1  2  3  4  5 
    ## 10 30 49 59 33

``` r
## Missing Data Calculations ##
table(is.na(spss.data))
```

    ## 
    ## FALSE  TRUE 
    ##  1807     3

``` r
# FALSE  TRUE 
#  1807     3

# 3 / (3 + 1807) = 0.001657459
# 0.001657459 * 100 = 0.1657459

## Less than 1% missing data, proceeding with complete case analyses
```

``` r
car::scatterplotMatrix(spss.data, smooth = F, regLine = F, col = 'black')
```

![](SPSSattitudesCode_files/figure-gfm/Scatterplot%20matrix-1.png)<!-- -->
\#\# Correlations

``` r
poly.spss.data <- psych::polychoric(spss.data) # wants numeric data
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

``` r
# 44 cells were adjusted for 0 values using the correction for continuity. Examine your data carefully.Call: psych::polychoric(x = spss.data)
# Polychoric correlations 
#         SPSS1E SPSS2 SPSS3 SPSS4 SPSS5 SPSS6 SPSS7 SPSS8 SPSS9 SPSS10
# SPSS1E  1.00                                                         
# SPSS2E  0.37   1.00                                                  
# SPSS3E  0.34   0.66  1.00                                            
# SPSS4E  0.76   0.30  0.35  1.00                                      
# SPSS5E  0.67   0.34  0.27  0.66  1.00                                
# SPSS6E  0.52   0.27  0.23  0.52  0.56  1.00                          
# SPSS7E  0.52   0.25  0.14  0.53  0.51  0.44  1.00                    
# SPSS8E  0.71   0.32  0.35  0.66  0.67  0.57  0.54  1.00              
# SPSS9E  0.74   0.29  0.34  0.76  0.65  0.58  0.56  0.71  1.00        
# SPSS10E 0.26   0.49  0.50  0.23  0.13  0.06  0.03  0.23  0.12  1.00  
# 
#  with tau of 
#            1     2      3    4
# SPSS1E  -1.7 -0.99 -0.084 0.97
# SPSS2E  -1.8 -0.89 -0.361 0.66
# SPSS3E  -1.6 -0.88 -0.182 0.90
# SPSS4E  -1.8 -0.89  0.035 1.20
# SPSS5E  -1.8 -1.19 -0.239 0.66
# SPSS6E  -1.4 -0.91 -0.090 0.64
# SPSS7E  -1.6 -0.59  0.104 0.87
# SPSS8E  -1.9 -1.14 -0.132 0.93
# SPSS9E  -1.8 -0.95  0.062 0.99
# SPSS10E -1.6 -0.77 -0.021 0.91

####
# Used polychoric bc Likert data
# All Qs are more correlated w eachother than they are with 2,3,10. But 2,3, and 10 are more correlated to each other than other Qs.
####
```

# MAP / PA

``` r
VSS(spss.data, fm = 'minres', cor = 'poly', plot = F)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## 
    ## Very Simple Structure
    ## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
    ##     n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
    ## VSS complexity 1 achieves a maximimum of 0.85  with  1  factors
    ## VSS complexity 2 achieves a maximimum of 0.94  with  2  factors
    ## 
    ## The Velicer MAP achieves a minimum of 0.04  with  2  factors 
    ## BIC achieves a minimum of  NA  with  2  factors
    ## Sample Size adjusted BIC achieves a minimum of  NA  with  3  factors
    ## 
    ## Statistics by number of factors 
    ##   vss1 vss2   map dof   chisq    prob sqresid  fit RMSEA BIC SABIC complex
    ## 1 0.85 0.00 0.047  35 1.8e+02 2.1e-21    4.52 0.85 0.151  -2 108.8     1.0
    ## 2 0.84 0.94 0.040  26 4.4e+01 1.4e-02    1.79 0.94 0.062 -91  -8.6     1.1
    ## 3 0.84 0.93 0.061  18 2.5e+01 1.3e-01    1.48 0.95 0.046 -69 -11.7     1.3
    ## 4 0.81 0.89 0.112  11 1.4e+01 2.2e-01    1.21 0.96 0.040 -43  -8.2     1.3
    ## 5 0.79 0.86 0.161   5 3.4e+00 6.4e-01    1.10 0.96 0.000 -23  -6.7     1.3
    ## 6 0.63 0.88 0.216   0 2.8e-01      NA    1.02 0.97    NA  NA    NA     1.6
    ## 7 0.84 0.90 0.322  -4 2.8e-07      NA    1.03 0.97    NA  NA    NA     1.5
    ## 8 0.84 0.91 0.463  -7 4.1e-11      NA    0.97 0.97    NA  NA    NA     1.5
    ##    eChisq    SRMR eCRMS eBIC
    ## 1 2.2e+02 1.2e-01 0.131   37
    ## 2 1.3e+01 2.8e-02 0.037 -122
    ## 3 5.5e+00 1.8e-02 0.029  -88
    ## 4 2.9e+00 1.3e-02 0.027  -54
    ## 5 7.7e-01 6.9e-03 0.021  -25
    ## 6 5.6e-02 1.8e-03    NA   NA
    ## 7 6.4e-08 2.0e-06    NA   NA
    ## 8 6.1e-12 1.9e-08    NA   NA

``` r
# Very Simple Structure
# Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
#     n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
# VSS complexity 1 achieves a maximimum of 0.85  with  1  factors
# VSS complexity 2 achieves a maximimum of 0.94  with  2  factors
# 
# The Velicer MAP achieves a minimum of 0.04  with  2  factors 
# BIC achieves a minimum of  NA  with  2  factors
# Sample Size adjusted BIC achieves a minimum of  NA  with  3  factors
# 
# Statistics by number of factors 

fa.parallel(spss.data, fm = 'minres', cor = 'poly', fa ='both', n.iter=100)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 39 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 39 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 39 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 39 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 39 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.
    
    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 40 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.
    
    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 38 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 45 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 41 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 42 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 43 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

![](SPSSattitudesCode_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  2  and the number of components =  2

``` r
# Parallel analysis suggests that the number of factors =  2  and the number of components =  2 

####
# Both MAP and PA suggest 2F
# PA should be interpreted w caution for polychoric
####

# Running 1F, 2F and 3F model (i.e. 1 above and 1 below suggested num. of factors) next to help determine which model is best
```

# EFAs

## 1F

``` r
fa(r = spss.data, fm = 'minres', rotate = "oblimin", cor = 'poly', nfactors = 1)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 1, rotate = "oblimin", fm = "minres", 
    ##     cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##          MR1   h2   u2 com
    ## SPSS1E  0.85 0.73 0.27   1
    ## SPSS2E  0.47 0.22 0.78   1
    ## SPSS3E  0.45 0.21 0.79   1
    ## SPSS4E  0.83 0.70 0.30   1
    ## SPSS5E  0.78 0.61 0.39   1
    ## SPSS6E  0.65 0.42 0.58   1
    ## SPSS7E  0.61 0.38 0.62   1
    ## SPSS8E  0.83 0.68 0.32   1
    ## SPSS9E  0.85 0.71 0.29   1
    ## SPSS10E 0.28 0.08 0.92   1
    ## 
    ##                 MR1
    ## SS loadings    4.74
    ## Proportion Var 0.47
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 35  and the objective function was  1.03 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.12 
    ## The df corrected root mean square of the residuals is  0.13 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  218.15  with prob <  2.5e-28 
    ## The total number of observations was  181  with Likelihood Chi Square =  179.94  with prob <  2.1e-21 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.81
    ## RMSEA index =  0.151  and the 90 % confidence intervals are  0.13 0.174
    ## BIC =  -2.01
    ## Fit based upon off diagonal values = 0.94
    ## Measures of factor score adequacy             
    ##                                                    MR1
    ## Correlation of (regression) scores with factors   0.96
    ## Multiple R square of scores with factors          0.93
    ## Minimum correlation of possible factor scores     0.86

``` r
# 44 cells were adjusted for 0 values using the correction for continuity. Examine your data carefully.Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 1, rotate = "oblimin", fm = "minres", 
#     cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# 
#                 MR1
# SS loadings    4.74
# Proportion Var 0.47
# 
# Mean item complexity =  1
# Test of the hypothesis that 1 factor is sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
# The degrees of freedom for the model are 35  and the objective function was  1.03 
# 
# The root mean square of the residuals (RMSR) is  0.12 
# The df corrected root mean square of the residuals is  0.13 
# 
# The harmonic number of observations is  180 with the empirical chi square  218.15  with prob <  2.5e-28 
# The total number of observations was  181  with Likelihood Chi Square =  179.94  with prob <  2.1e-21 
# 
# Tucker Lewis Index of factoring reliability =  0.81
# RMSEA index =  0.151  and the 90 % confidence intervals are  0.13 0.174
# BIC =  -2.01
# Fit based upon off diagonal values = 0.94
# Measures of factor score adequacy             
#                                                    MR1
# Correlation of (regression) scores with factors   0.96
# Multiple R square of scores with factors          0.93
# Minimum correlation of possible factor scores     0.86
# 
#         MR1       h2          u2    com
# SPSS1E    0.85    0.72856429  0.2714357   1     
# SPSS2E    0.47    0.21794717  0.7820528   1     *
# SPSS3E    0.45    0.20643367  0.7935663   1     *
# SPSS4E    0.83    0.69670807  0.3032919   1
# SPSS5E    0.78    0.60983397  0.3901660   1
# SPSS6E    0.65    0.42275333  0.5772467   1
# SPSS7E    0.61    0.37574147  0.6242585   1
# SPSS8E    0.83    0.68331094  0.3166891   1
# SPSS9E    0.85    0.71463561  0.2853644   1
# SPSS10E   0.28    0.08033225  0.9196677   1     **

####
# RMSR = 0.12 *BAD*
# Prop. var explained = 0.47 
# SPSS10E *BAD* factor loading (<.4) and communality (0.08)
# SPSS2E and SPSS3E factor loading almost <.4 and communality almost <.2
#
# Based on model fit (RMSR), 1F sucks
####
```

## 2F

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 2)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 2, fm = "minres", cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   h2   u2 com
    ## SPSS1E   0.81  0.08 0.72 0.28 1.0
    ## SPSS2E   0.06  0.76 0.62 0.38 1.0
    ## SPSS3E   0.02  0.82 0.68 0.32 1.0
    ## SPSS4E   0.82  0.04 0.70 0.30 1.0
    ## SPSS5E   0.80 -0.01 0.63 0.37 1.0
    ## SPSS6E   0.68 -0.04 0.44 0.56 1.0
    ## SPSS7E   0.69 -0.11 0.42 0.58 1.1
    ## SPSS8E   0.80  0.05 0.68 0.32 1.0
    ## SPSS9E   0.88 -0.04 0.75 0.25 1.0
    ## SPSS10E -0.09  0.66 0.40 0.60 1.0
    ## 
    ##                        MR1  MR2
    ## SS loadings           4.35 1.71
    ## Proportion Var        0.43 0.17
    ## Cumulative Var        0.43 0.61
    ## Proportion Explained  0.72 0.28
    ## Cumulative Proportion 0.72 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2
    ## MR1 1.00 0.44
    ## MR2 0.44 1.00
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 26  and the objective function was  0.25 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
    ## The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.968
    ## RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
    ## BIC =  -90.96
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2
    ## Correlation of (regression) scores with factors   0.96 0.91
    ## Multiple R square of scores with factors          0.93 0.82
    ## Minimum correlation of possible factor scores     0.86 0.65

``` r
# 44 cells were adjusted for 0 values using the correction for continuity. Examine your data carefully.Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 2, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# 
#                        MR1  MR2
# SS loadings           4.35 1.71
# Proportion Var        0.43 0.17
# Cumulative Var        0.43 0.61 *
# Proportion Explained  0.72 0.28
# Cumulative Proportion 0.72 1.00
# 
#  With factor correlations of 
#      MR1  MR2
# MR1 1.00 0.44
# MR2 0.44 1.00
# 
# Mean item complexity =  1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
# The degrees of freedom for the model are 26  and the objective function was  0.25 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
# The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
# 
# Tucker Lewis Index of factoring reliability =  0.968
# RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
# BIC =  -90.96
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
#                                                    MR1  MR2
# Correlation of (regression) scores with factors   0.96 0.91
# Multiple R square of scores with factors          0.93 0.82
# Minimum correlation of possible factor scores     0.86 0.65
# 
#         MR1   MR2       h2        u2    com
# SPSS1E    0.81    0.08    0.7236662   0.2763338   1.019276
# SPSS2E    0.06    0.76    0.6178306   0.3821694   1.012793
# SPSS3E    0.02    0.82    0.6827871   0.3172129   1.000986
# SPSS4E    0.82    0.04    0.7018363   0.2981637   1.004479
# SPSS5E    0.80    -0.01   0.6295613   0.3704387   1.000441
# SPSS6E    0.68    -0.04   0.4448130   0.5551870   1.006886
# SPSS7E    0.69    -0.11   0.4244661   0.5755339   1.054229
# SPSS8E    0.80    0.05    0.6842864   0.3157136   1.009224
# SPSS9E    0.88    -0.04   0.7508519   0.2491481   1.003669
# SPSS10E   -0.09   0.66    0.3975595   0.6024405   1.036934

####
# RMSR = 0.03 *WOW!* huge decrease by adding 1 more factor
# Prop. var explained = 0.61, 14% raw difference from 1F model
# No poor factor loadings or low communalities
# Column and row parsimony is pretty amazing
# Notice that all negatively worded items load onto factor 2 & all positively worded items load onto factor 1
# 2F prob wins, but let's try 3F next anyways
####
```

## 3F

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 3)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 3, fm = "minres", cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   MR3   h2     u2 com
    ## SPSS1E   0.84 -0.01  0.11 0.74 0.2596 1.0
    ## SPSS2E  -0.01  1.00  0.00 1.00 0.0043 1.0
    ## SPSS3E   0.15  0.47  0.36 0.57 0.4313 2.1
    ## SPSS4E   0.86 -0.07  0.12 0.73 0.2683 1.1
    ## SPSS5E   0.77  0.08 -0.11 0.63 0.3655 1.1
    ## SPSS6E   0.64  0.10 -0.18 0.47 0.5336 1.2
    ## SPSS7E   0.64  0.07 -0.22 0.45 0.5491 1.3
    ## SPSS8E   0.82  0.01  0.04 0.69 0.3130 1.0
    ## SPSS9E   0.88 -0.03 -0.02 0.75 0.2488 1.0
    ## SPSS10E  0.04  0.27  0.56 0.51 0.4937 1.5
    ## 
    ##                        MR1  MR2  MR3
    ## SS loadings           4.39 1.49 0.65
    ## Proportion Var        0.44 0.15 0.07
    ## Cumulative Var        0.44 0.59 0.65
    ## Proportion Explained  0.67 0.23 0.10
    ## Cumulative Proportion 0.67 0.90 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2  MR3
    ## MR1 1.00 0.39 0.13
    ## MR2 0.39 1.00 0.37
    ## MR3 0.13 0.37 1.00
    ## 
    ## Mean item complexity =  1.2
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 18  and the objective function was  0.14 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.02 
    ## The df corrected root mean square of the residuals is  0.03 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  5.49  with prob <  1 
    ## The total number of observations was  181  with Likelihood Chi Square =  24.9  with prob <  0.13 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.982
    ## RMSEA index =  0.046  and the 90 % confidence intervals are  0 0.086
    ## BIC =  -68.68
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1 MR2  MR3
    ## Correlation of (regression) scores with factors   0.97   1 0.76
    ## Multiple R square of scores with factors          0.93   1 0.58
    ## Minimum correlation of possible factor scores     0.87   1 0.16

``` r
# 44 cells were adjusted for 0 values using the correction for continuity. Examine your data carefully.Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 3, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# 
#                        MR1  MR2  MR3
# SS loadings           4.39 1.49 0.65
# Proportion Var        0.44 0.15 0.07
# Cumulative Var        0.44 0.59 0.65 *
# Proportion Explained  0.67 0.23 0.10
# Cumulative Proportion 0.67 0.90 1.00
# 
#  With factor correlations of 
#      MR1  MR2  MR3
# MR1 1.00 0.39 0.13
# MR2 0.39 1.00 0.37
# MR3 0.13 0.37 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
# The degrees of freedom for the model are 18  and the objective function was  0.14 
# 
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.03 
# 
# The harmonic number of observations is  180 with the empirical chi square  5.49  with prob <  1 
# The total number of observations was  181  with Likelihood Chi Square =  24.9  with prob <  0.13 
# 
# Tucker Lewis Index of factoring reliability =  0.982
# RMSEA index =  0.046  and the 90 % confidence intervals are  0 0.086
# BIC =  -68.68
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
#                                                    MR1 MR2  MR3
# Correlation of (regression) scores with factors   0.97   1 0.76
# Multiple R square of scores with factors          0.93   1 0.58
# Minimum correlation of possible factor scores     0.87   1 0.16
# 
#         MR1   MR2   MR3       h2        u2      com
# SPSS1E    0.84    -0.01   0.11    0.7403837   0.259616297 1.036000
# SPSS2E    -0.01   1.00    0.00    0.9956504   0.004349638 1.000068
# SPSS3E    0.15    0.47    0.36    0.5686618   0.431338226 2.084417
# SPSS4E    0.86    -0.07   0.12    0.7317213   0.268278681 1.055702
# SPSS5E    0.77    0.08    -0.11   0.6345384   0.365461572 1.067785
# SPSS6E    0.64    0.10    -0.18   0.4664426   0.533557360 1.205680
# SPSS7E    0.64    0.07    -0.22   0.4508917   0.549108279 1.270510
# SPSS8E    0.82    0.01    0.04    0.6870119   0.312988095 1.006366
# SPSS9E    0.88    -0.03   -0.02   0.7511585   0.248841506 1.003480
# SPSS10E   0.04    0.27    0.56    0.5062815   0.493718467 1.455608

####
# RMSR = 0.02 *MEH* only decreased by 0.01 after adding an additional factor - not worth it bc RMSR always decreases when adding an additional factor.
# Prop. var explained = 0.65, 4% raw difference from 2F model
# No low communalities, BUT
  # SPSS3E has a cross-loading
  # In general, column and row parsimony is not nearly as good as 2F model
  # A question: SPSS2E loads onto MR2 at 1.00(!)
# Concluding that 2F wins bc improvements in model fit isn't worth it & column and row parsimony worse than 2F model 
####
```

# 2F with rotations

<!-- The original with oblimin: -->

<!-- #         MR1   MR2       h2        u2    com -->

<!-- # SPSS1E   0.81    0.08    0.7236662   0.2763338   1.019276 -->

<!-- # SPSS2E   0.06    0.76    0.6178306   0.3821694   1.012793 -->

<!-- # SPSS3E   0.02    0.82    0.6827871   0.3172129   1.000986 -->

<!-- # SPSS4E   0.82    0.04    0.7018363   0.2981637   1.004479 -->

<!-- # SPSS5E   0.80    -0.01   0.6295613   0.3704387   1.000441 -->

<!-- # SPSS6E   0.68    -0.04   0.4448130   0.5551870   1.006886 -->

<!-- # SPSS7E   0.69    -0.11   0.4244661   0.5755339   1.054229 -->

<!-- # SPSS8E   0.80    0.05    0.6842864   0.3157136   1.009224 -->

<!-- # SPSS9E   0.88    -0.04   0.7508519   0.2491481   1.003669 -->

<!-- # SPSS10E  -0.09   0.66    0.3975595   0.6024405   1.036934 -->

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'bentlerQ', nfactors = 2)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 2, rotate = "bentlerQ", fm = "minres", 
    ##     cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   h2   u2 com
    ## SPSS1E   0.81  0.08 0.72 0.28 1.0
    ## SPSS2E   0.05  0.76 0.62 0.38 1.0
    ## SPSS3E   0.01  0.82 0.68 0.32 1.0
    ## SPSS4E   0.82  0.04 0.70 0.30 1.0
    ## SPSS5E   0.80 -0.01 0.63 0.37 1.0
    ## SPSS6E   0.68 -0.04 0.44 0.56 1.0
    ## SPSS7E   0.70 -0.12 0.42 0.58 1.1
    ## SPSS8E   0.80  0.05 0.68 0.32 1.0
    ## SPSS9E   0.88 -0.04 0.75 0.25 1.0
    ## SPSS10E -0.10  0.67 0.40 0.60 1.0
    ## 
    ##                        MR1  MR2
    ## SS loadings           4.35 1.71
    ## Proportion Var        0.43 0.17
    ## Cumulative Var        0.43 0.61
    ## Proportion Explained  0.72 0.28
    ## Cumulative Proportion 0.72 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2
    ## MR1 1.00 0.45
    ## MR2 0.45 1.00
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 26  and the objective function was  0.25 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
    ## The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.968
    ## RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
    ## BIC =  -90.96
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2
    ## Correlation of (regression) scores with factors   0.96 0.91
    ## Multiple R square of scores with factors          0.93 0.82
    ## Minimum correlation of possible factor scores     0.86 0.65

``` r
#          MR1   MR2       h2        u2    com
# SPSS1E    0.81    0.08    0.7236662   0.2763338   1.018186
# SPSS2E    0.05    0.76    0.6178306   0.3821694   1.010090
# SPSS3E    0.01    0.82    0.6827871   0.3172129   1.000358
# SPSS4E    0.82    0.04    0.7018363   0.2981637   1.003928
# SPSS5E    0.80    -0.01   0.6295613   0.3704387   1.000650
# SPSS6E    0.68    -0.04   0.4448130   0.5551870   1.007671
# SPSS7E    0.70    -0.12   0.4244661   0.5755339   1.056426
# SPSS8E    0.80    0.05    0.6842864   0.3157136   1.008445
# SPSS9E    0.88    -0.04   0.7508519   0.2491481   1.004244
# SPSS10E   -0.10   0.67    0.3975595   0.6024405   1.041620

####
# Very close to begin identifical to oblimin
####
```

## GeominQ

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'geominQ', nfactors = 2)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 2, rotate = "geominQ", fm = "minres", 
    ##     cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   h2   u2 com
    ## SPSS1E   0.81  0.09 0.72 0.28   1
    ## SPSS2E   0.06  0.76 0.62 0.38   1
    ## SPSS3E   0.02  0.82 0.68 0.32   1
    ## SPSS4E   0.82  0.05 0.70 0.30   1
    ## SPSS5E   0.80  0.00 0.63 0.37   1
    ## SPSS6E   0.68 -0.03 0.44 0.56   1
    ## SPSS7E   0.69 -0.11 0.42 0.58   1
    ## SPSS8E   0.80  0.06 0.68 0.32   1
    ## SPSS9E   0.88 -0.03 0.75 0.25   1
    ## SPSS10E -0.09  0.66 0.40 0.60   1
    ## 
    ##                        MR1  MR2
    ## SS loadings           4.34 1.72
    ## Proportion Var        0.43 0.17
    ## Cumulative Var        0.43 0.61
    ## Proportion Explained  0.72 0.28
    ## Cumulative Proportion 0.72 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2
    ## MR1 1.00 0.42
    ## MR2 0.42 1.00
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 26  and the objective function was  0.25 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
    ## The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.968
    ## RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
    ## BIC =  -90.96
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2
    ## Correlation of (regression) scores with factors   0.96 0.91
    ## Multiple R square of scores with factors          0.93 0.82
    ## Minimum correlation of possible factor scores     0.86 0.64

``` r
#           MR1   MR2       h2        u2    com
# SPSS1E    0.81    0.09    0.7236662   0.2763338   1.022965
# SPSS2E    0.06    0.76    0.6178306   0.3821694   1.014742
# SPSS3E    0.02    0.82    0.6827871   0.3172129   1.001591
# SPSS4E    0.82    0.05    0.7018363   0.2981637   1.006339
# SPSS5E    0.80    0.00    0.6295613   0.3704387   1.000070
# SPSS6E    0.68    -0.03   0.4448130   0.5551870   1.004967
# SPSS7E    0.69    -0.11   0.4244661   0.5755339   1.048740
# SPSS8E    0.80    0.06    0.6842864   0.3157136   1.011826
# SPSS9E    0.88    -0.03   0.7508519   0.2491481   1.002307
# SPSS10E   -0.09   0.66    0.3975595   0.6024405   1.033722

####
# Identical to oblimin
####
```

## Quartimin

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "quartimin", nfactors = 2)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 2, rotate = "quartimin", fm = "minres", 
    ##     cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   h2   u2 com
    ## SPSS1E   0.81  0.08 0.72 0.28 1.0
    ## SPSS2E   0.06  0.76 0.62 0.38 1.0
    ## SPSS3E   0.02  0.82 0.68 0.32 1.0
    ## SPSS4E   0.82  0.04 0.70 0.30 1.0
    ## SPSS5E   0.80 -0.01 0.63 0.37 1.0
    ## SPSS6E   0.68 -0.04 0.44 0.56 1.0
    ## SPSS7E   0.69 -0.11 0.42 0.58 1.1
    ## SPSS8E   0.80  0.05 0.68 0.32 1.0
    ## SPSS9E   0.88 -0.04 0.75 0.25 1.0
    ## SPSS10E -0.09  0.66 0.40 0.60 1.0
    ## 
    ##                        MR1  MR2
    ## SS loadings           4.35 1.71
    ## Proportion Var        0.43 0.17
    ## Cumulative Var        0.43 0.61
    ## Proportion Explained  0.72 0.28
    ## Cumulative Proportion 0.72 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2
    ## MR1 1.00 0.44
    ## MR2 0.44 1.00
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 26  and the objective function was  0.25 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
    ## The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.968
    ## RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
    ## BIC =  -90.96
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2
    ## Correlation of (regression) scores with factors   0.96 0.91
    ## Multiple R square of scores with factors          0.93 0.82
    ## Minimum correlation of possible factor scores     0.86 0.65

``` r
#          MR1   MR2       h2        u2    com
# SPSS1E    0.81    0.08    0.7236662   0.2763338   1.019276
# SPSS2E    0.06    0.76    0.6178306   0.3821694   1.012793
# SPSS3E    0.02    0.82    0.6827871   0.3172129   1.000986
# SPSS4E    0.82    0.04    0.7018363   0.2981637   1.004479
# SPSS5E    0.80    -0.01   0.6295613   0.3704387   1.000441
# SPSS6E    0.68    -0.04   0.4448130   0.5551870   1.006886
# SPSS7E    0.69    -0.11   0.4244661   0.5755339   1.054229
# SPSS8E    0.80    0.05    0.6842864   0.3157136   1.009224
# SPSS9E    0.88    -0.04   0.7508519   0.2491481   1.003669
# SPSS10E   -0.09   0.66    0.3975595   0.6024405   1.036934

####
# Identical to oblimin
####
```

## Promax

``` r
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "Promax", nfactors = 2)
```

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 44 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

    ## Factor Analysis using method =  minres
    ## Call: fa(r = spss.data, nfactors = 2, rotate = "Promax", fm = "minres", 
    ##     cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           MR1   MR2   h2   u2 com
    ## SPSS1E   0.81  0.09 0.72 0.28   1
    ## SPSS2E   0.09  0.75 0.62 0.38   1
    ## SPSS3E   0.05  0.81 0.68 0.32   1
    ## SPSS4E   0.82  0.05 0.70 0.30   1
    ## SPSS5E   0.79  0.00 0.63 0.37   1
    ## SPSS6E   0.68 -0.03 0.44 0.56   1
    ## SPSS7E   0.69 -0.10 0.42 0.58   1
    ## SPSS8E   0.80  0.07 0.68 0.32   1
    ## SPSS9E   0.88 -0.02 0.75 0.25   1
    ## SPSS10E -0.07  0.65 0.40 0.60   1
    ## 
    ##                        MR1  MR2
    ## SS loadings           4.35 1.71
    ## Proportion Var        0.43 0.17
    ## Cumulative Var        0.43 0.61
    ## Proportion Explained  0.72 0.28
    ## Cumulative Proportion 0.72 1.00
    ## 
    ##  With factor correlations of 
    ##      MR1  MR2
    ## MR1 1.00 0.39
    ## MR2 0.39 1.00
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  5.86 with Chi Square of  1030.98
    ## The degrees of freedom for the model are 26  and the objective function was  0.25 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  180 with the empirical chi square  13.09  with prob <  0.98 
    ## The total number of observations was  181  with Likelihood Chi Square =  44.2  with prob <  0.014 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.968
    ## RMSEA index =  0.062  and the 90 % confidence intervals are  0.028 0.093
    ## BIC =  -90.96
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2
    ## Correlation of (regression) scores with factors   0.96 0.90
    ## Multiple R square of scores with factors          0.93 0.82
    ## Minimum correlation of possible factor scores     0.86 0.64

``` r
#         MR1   MR2       h2        u2    com
# SPSS1E    0.81    0.09    0.7236662   0.2763338   1.025101
# SPSS2E    0.09    0.75    0.6178306   0.3821694   1.026425
# SPSS3E    0.05    0.81    0.6827871   0.3172129   1.006460
# SPSS4E    0.82    0.05    0.7018363   0.2981637   1.007655
# SPSS5E    0.79    0.00    0.6295613   0.3704387   1.000000
# SPSS6E    0.68    -0.03   0.4448130   0.5551870   1.003746
# SPSS7E    0.69    -0.10   0.4244661   0.5755339   1.044521
# SPSS8E    0.80    0.07    0.6842864   0.3157136   1.013513
# SPSS9E    0.88    -0.02   0.7508519   0.2491481   1.001514
# SPSS10E   -0.07   0.65    0.3975595   0.6024405   1.021111

####
# Not very differnt from oblimin, if anything - .01 worse than oblimin
####
```

I quickly ran the remaining oblique rotations mentioned in the ‘fa’
function’s documentation. They’re all nearly identical to oblimin as the
ones above.

# Bifactor model

``` r
psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = 'minres', rotate = 'oblimin', na.obs = NA, option = 'equal')
```

    ## 
    ## Three factors are required for identification -- general factor loadings set to be equal. 
    ## Proceed with caution. 
    ## Think about redoing the analysis with alternative values of the 'option' setting.

    ## Schmid-Leiman analysis 
    ## Call: psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = "minres", 
    ##     rotate = "oblimin", option = "equal", na.obs = NA)
    ## 
    ## Schmid Leiman Factor loadings greater than  0.2 
    ##            g   F1*   F2*   h2   u2   p2
    ## SPSS1E  0.59  0.61       0.72 0.28 0.48
    ## SPSS2E  0.54        0.57 0.62 0.38 0.47
    ## SPSS3E  0.55        0.61 0.68 0.32 0.45
    ## SPSS4E  0.57  0.62       0.70 0.30 0.46
    ## SPSS5E  0.52  0.60       0.63 0.37 0.43
    ## SPSS6E  0.42  0.51       0.44 0.56 0.41
    ## SPSS7E  0.38  0.52       0.42 0.58 0.34
    ## SPSS8E  0.57  0.60       0.68 0.32 0.47
    ## SPSS9E  0.56  0.66       0.75 0.25 0.41
    ## SPSS10E 0.38        0.50 0.40 0.60 0.36
    ## 
    ## With eigenvalues of:
    ##    g  F1*  F2* 
    ## 2.63 2.46 0.97 
    ## 
    ## general/max  1.07   max/min =   2.54
    ## mean percent general =  0.43    with sd =  0.05 and cv of  0.11 
    ## 
    ##  The orthogonal loadings were 
    ## Unstandardized loadings based upon covariance matrix
    ##           F1   F2   h2   u2   H2   U2
    ## SPSS1E  0.81 0.26 0.72 0.28 0.72 0.28
    ## SPSS2E  0.22 0.75 0.62 0.38 0.62 0.38
    ## SPSS3E  0.20 0.80 0.68 0.32 0.68 0.32
    ## SPSS4E  0.81 0.23 0.70 0.30 0.70 0.30
    ## SPSS5E  0.77 0.17 0.63 0.37 0.63 0.37
    ## SPSS6E  0.66 0.12 0.44 0.56 0.44 0.56
    ## SPSS7E  0.65 0.05 0.42 0.58 0.42 0.58
    ## SPSS8E  0.79 0.24 0.68 0.32 0.68 0.32
    ## SPSS9E  0.85 0.17 0.75 0.25 0.75 0.25
    ## SPSS10E 0.06 0.63 0.40 0.60 0.40 0.60
    ## 
    ##                  F1   F2
    ## SS loadings    4.20 1.86
    ## Proportion Var 0.42 0.19
    ## Cumulative Var 0.42 0.60
    ## 
    ## The degrees of freedom are 26  and the fit is  0.25 
    ## 
    ## The root mean square of the residuals is  0.03 
    ## The df corrected root mean square of the residuals is  0.04

``` r
#### Carmen's random notes:
  
# nfactors: number of factors to extract. Note that it already assumes the general factor
# What is digits??
# n.obs is calculated if input is raw data. "NA" is default.
# 3 factors are the minimum number necessary to define the solution uniquely, but 2F can be done.
  # A message is issued suggesting that the model is not really well defined.
  # There are three possible options for this condition: setting the general factor loadings between the two lower order factors to be "equal" which will be the sqrt(oblique correlations between the factors) or to "first" or "second" in which case the general factor is equated with either the first or second group factor.

#### Actual output below:

# Three factors are required for identification -- general factor loadings set to be equal. 
# Proceed with caution. 
# Think about redoing the analysis with alternative values of the 'option' setting.
# 
# Schmid-Leiman analysis 
# Call: psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = "minres", 
#     rotate = "oblimin", option = "equal", na.obs = NA)
# 
# Schmid Leiman Factor loadings greater than  0.2 
# 
# With eigenvalues of:
#    g  F1*  F2* 
# 2.63 2.46 0.97 
# 
# general/max  1.07   max/min =   2.54
# mean percent general =  0.43    with sd =  0.05 and cv of  0.11 
# 
#  The orthogonal loadings were 
# Unstandardized loadings based upon covariance matrix
# 
#                  F1   F2
# SS loadings    4.20 1.86
# Proportion Var 0.42 0.19
# Cumulative Var 0.42 0.60
# 
# The degrees of freedom are 26  and the fit is  0.25 
# 
# The root mean square of the residuals is  0.03 
# The df corrected root mean square of the residuals is  0.04
# 
#         g     F1*   F2*    h2    u2    p2
# SPSS1E    0.59    0.61            0.72    0.28    0.48
# SPSS2E    0.54            0.57    0.62    0.38    0.47
# SPSS3E    0.55            0.61    0.68    0.32    0.45
# SPSS4E    0.57    0.62            0.70    0.30    0.46
# SPSS5E    0.52    0.60            0.63    0.37    0.43
# SPSS6E    0.42    0.51            0.44    0.56    0.41
# SPSS7E    0.38    0.52            0.42    0.58    0.34
# SPSS8E    0.57    0.60            0.68    0.32    0.47
# SPSS9E    0.56    0.66            0.75    0.25    0.41
# SPSS10E   0.38            0.50    0.40    0.60    0.36
# 
#         F1    F2    h2        u2    H2        U2
# SPSS1E    0.81    0.26    0.7236662   0.28    0.7210228   0.2789772
# SPSS2E    0.22    0.75    0.6178306   0.38    0.6191738   0.3808262
# SPSS3E    0.20    0.80    0.6827871   0.32    0.6808894   0.3191106
# SPSS4E    0.81    0.23    0.7018363   0.30    0.7005499   0.2994501
# SPSS5E    0.77    0.17    0.6295613   0.37    0.6298376   0.3701624
# SPSS6E    0.66    0.12    0.4448130   0.56    0.4426824   0.5573176
# SPSS7E    0.65    0.05    0.4244661   0.58    0.4225788   0.5774212
# SPSS8E    0.79    0.24    0.6842864   0.32    0.6813658   0.3186342
# SPSS9E    0.85    0.17    0.7508519   0.25    0.7502128   0.2497872
# SPSS10E   0.06    0.63    0.3975595   0.60    0.3985321   0.6014679

####
# *NOTICE* the warning - that 3 factors are required for identification - what does this mean? Can we interpret model fit stats? Below are my interpretations (ignoring this warning)

# Below is based on R console output:
# 'g' and 'F1*' have eigenvals > 1 - F2 is slightly < 1.
# Prop. var explained = .60 *AS GOOD AS 2F MODEL*
# RMSR: .03 *GOOD*

# Below is based off the first df in output (i.e. "sl")
  # All positive valenced items load onto F1 and all negative valenced items load onto F2 (good)
  # All questions load onto 'g' (i.e. general factor) OK except SPSS10E and SPSS7E (could be better)
    # ...to be discussed why (are these bad items? if so, why?)

# I think the second df in output is "orthog" (i.e. original orthogonal factor loadings - ignore)
####
```

# Assumptions

1.  MV normality - N/A because we are using minres/ULS and not ML
2.  Multicollinearity - not violated if the model converges (which it
    does)
3.  Linearity - N/A bc polychoric correlations are used