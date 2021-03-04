Validation Code for Attitudes toward SPSS (ATSPSS)
================
Alter, Dang, Kunicki, Counsell
Jan 31, 2021

## Set-up

``` r
# Packages ----------------------------------------------------------------
```

``` r
library(tidyverse)
library(car)
library(psych)
library(mirt) # for multiple IRT
library(GPArotation) # for Promax rotation
library(REdaS) # for assumptions
library(faoutlier) # for efa outliers
library(mice) # for imputations
```

## Initial Data Setup

``` r
# Initial Data Setup ------------------------------------------------------
```

**Uploading raw data**

``` r
full.data <- readxl::read_xlsx("spssdata.xlsx", col_names = TRUE)
# Remove empty rows
full.data <- full.data[1:181, ]
```

**Selecting only SPSS-related columns**

``` r
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
```

**Reverse code SPSS items 2, 3, 10**

``` r
spss.data$SPSS2E <- car::recode(spss.data$SPSS2E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS3E <- car::recode(spss.data$SPSS3E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
spss.data$SPSS10E <- car::recode(spss.data$SPSS10E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
```

## Descriptive Statistics

``` r
# Descriptive Stats -------------------------------------------------------
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

**Contingency table of the counts**

``` r
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

**Missing Data Calculations**

``` r
table(is.na(spss.data))
```

    ## 
    ## FALSE  TRUE 
    ##  1807     3

``` r
# MISSING / (MISSING + NOT MISSING) = PERCENT MISSING 
# 3 / (3 + 1807) = 0.001657459
# 0.001657459 * 100 = 0.1657459

# Less than 1% missing data, proceeding with complete case analyses
```

**Scatterplot Matrix**

``` r
car::scatterplotMatrix(spss.data, smooth = F, regLine = F, col = 'black')
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Statistical Assumptions

``` r
# Statistical Assumptions -------------------------------------------------
```

**Multivariate Normality**

``` r
mardia(spss.data) # Kurtosis = 15.17 >4. Will not assume mvn.
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    ## Call: mardia(x = spss.data)
    ## 
    ## Mardia tests of multivariate skew and kurtosis
    ## Use describe(x) the to get univariate tests
    ## n.obs = 178   num.vars =  10 
    ## b1p =  19.29   skew =  572.2  with probability =  0
    ##  small sample skew =  583.62  with probability =  0
    ## b2p =  155.23   kurtosis =  15.17  with probability =  0

**EFA Appropriateness**

``` r
# Barlett's Test of Sphericity tests whether a matrix is significantly different from an identity matrix
bart_spher(spss.data, use = "complete.obs") # p-value < 2.22e-16
```

    ##  Bartlett's Test of Sphericity
    ## 
    ## Call: bart_spher(x = spss.data, use = "complete.obs")
    ## 
    ##      X2 = 969.939
    ##      df = 45
    ## p-value < 2.22e-16

    ## Warning: Used n = 178.

**Kaiser-Meyer-Olkin Statistics**

``` r
KMOS(spss.data, use = "complete.obs")
```

    ## 
    ## Kaiser-Meyer-Olkin Statistics
    ## 
    ## Call: KMOS(x = spss.data, use = "complete.obs")
    ## 
    ## Measures of Sampling Adequacy (MSA):
    ##    SPSS1E    SPSS2E    SPSS3E    SPSS4E    SPSS5E    SPSS6E    SPSS7E    SPSS8E 
    ## 0.9098933 0.7138465 0.7359144 0.8758975 0.9281158 0.9543978 0.9378408 0.9022414 
    ##    SPSS9E   SPSS10E 
    ## 0.8956706 0.6953167 
    ## 
    ## KMO-Criterion: 0.8795382

## Listwise Deletion / Complete Case Analysis

``` r
# Listwise Deletion / Complete Case Analysis ------------------------------
spss.data.withNA <- spss.data # spss data without removing missing values, for imputations 

# Previous work suggests using listwise deletion when the missing data rates are extremely low (e.g., < 1%; Flora, 2018; Jakobsen et al., 2017).
spss.data <- spss.data[-c(33, 141, 104), ]
full.data <- full.data[-c(33, 141, 104), ] # needed later for convergent/discriminant validity

spss.data <- data.frame(spss.data)
str(spss.data)
```

    ## 'data.frame':    178 obs. of  10 variables:
    ##  $ SPSS1E : num  4 4 3 2 4 2 4 2 3 4 ...
    ##  $ SPSS2E : num  4 5 3 4 5 2 3 2 4 5 ...
    ##  $ SPSS3E : num  4 5 3 4 4 2 1 2 5 5 ...
    ##  $ SPSS4E : num  3 4 2 2 3 3 2 3 3 4 ...
    ##  $ SPSS5E : num  3 4 3 2 4 2 4 3 3 3 ...
    ##  $ SPSS6E : num  3 4 2 2 4 1 4 3 1 5 ...
    ##  $ SPSS7E : num  4 4 2 2 3 3 2 3 4 4 ...
    ##  $ SPSS8E : num  4 4 3 2 4 3 3 3 3 2 ...
    ##  $ SPSS9E : num  4 4 3 2 3 3 2 2 2 4 ...
    ##  $ SPSS10E: num  4 5 3 4 5 3 3 3 4 3 ...

## Polychoric Correlations

``` r
# Polychoric Correlations -------------------------------------------------
```

Using polychoric correlations because the data is categorical (5-point
Likert scale)

``` r
poly.spss.data <- psych::polychoric(spss.data)

# Confidence Intervals for Polychoric Correlations
(cor.ci(spss.data, poly = TRUE, plot = FALSE))$ci
```

    ## Warning in cor.smooth(mat): Matrix was not positive definite, smoothing was done

    ##                      lower        low.e     upper      up.e            p
    ## SPSS1E-SPSS2   0.160276769  0.179082140 0.5579618 0.5530159 1.628678e-03
    ## SPSS1E-SPSS3   0.186080418  0.184630447 0.5120240 0.5444658 1.832165e-04
    ## SPSS1E-SPSS4   0.775203933  0.797678851 0.9222432 0.9277895 2.645117e-09
    ## SPSS1E-SPSS5   0.580141893  0.589024948 0.8224001 0.8240478 1.610307e-08
    ## SPSS1E-SPSS6   0.427353044  0.431835363 0.6706235 0.6659390 5.996461e-10
    ## SPSS1E-SPSS7   0.412606224  0.422781221 0.6774286 0.6630702 1.316667e-08
    ## SPSS1E-SPSS8   0.685931148  0.696267543 0.8662875 0.8576360 7.762413e-11
    ## SPSS1E-SPSS9   0.721647836  0.729712814 0.8910153 0.8913401 3.795579e-10
    ## SPSS1E-SPSS10  0.047092447  0.059127935 0.4259574 0.3846658 1.810298e-02
    ## SPSS2-SPSS3    0.552052739  0.578573511 0.8057753 0.8030813 2.651696e-08
    ## SPSS2-SPSS4    0.119777389  0.146121883 0.4939452 0.5102536 2.947546e-03
    ## SPSS2-SPSS5    0.164000796  0.170858729 0.5204012 0.5230084 7.172285e-04
    ## SPSS2-SPSS6    0.087167125  0.077581367 0.4344254 0.4183002 5.181762e-03
    ## SPSS2-SPSS7    0.041977249  0.002507876 0.3968045 0.3699906 1.857211e-02
    ## SPSS2-SPSS8    0.120095295  0.104354778 0.4895279 0.4475953 2.754770e-03
    ## SPSS2-SPSS9    0.103286553  0.131752779 0.4860850 0.4661219 4.846700e-03
    ## SPSS2-SPSS10   0.318290143  0.329490648 0.6591298 0.6479111 1.577182e-05
    ## SPSS3-SPSS4    0.197527389  0.229660636 0.5349691 0.5663862 1.835607e-04
    ## SPSS3-SPSS5    0.112997669  0.127788937 0.4512748 0.4476987 2.199296e-03
    ## SPSS3-SPSS6    0.058205914  0.057829075 0.3893122 0.3781991 1.044469e-02
    ## SPSS3-SPSS7   -0.071439042 -0.085247335 0.3181320 0.2955365 2.099491e-01
    ## SPSS3-SPSS8    0.193864693  0.201974550 0.5039814 0.5025349 8.656818e-05
    ## SPSS3-SPSS9    0.185129265  0.184915686 0.5252277 0.5171366 2.784554e-04
    ## SPSS3-SPSS10   0.321193405  0.327783199 0.6306194 0.6335749 2.565059e-06
    ## SPSS4-SPSS5    0.596040952  0.587269786 0.8580891 0.8644143 7.576783e-07
    ## SPSS4-SPSS6    0.421904796  0.445428762 0.6929145 0.6846505 2.624588e-08
    ## SPSS4-SPSS7    0.402795879  0.442676880 0.6845168 0.6832572 9.153150e-08
    ## SPSS4-SPSS8    0.637947232  0.646680358 0.8436450 0.8406041 5.206078e-10
    ## SPSS4-SPSS9    0.766441580  0.793740069 0.9283741 0.9323221 8.054987e-08
    ## SPSS4-SPSS10  -0.009022745 -0.029685784 0.4177829 0.3869456 6.391011e-02
    ## SPSS5-SPSS6    0.421109683  0.448616620 0.7157178 0.7084128 3.042553e-07
    ## SPSS5-SPSS7    0.423719742  0.452072456 0.6617137 0.6596533 2.636571e-10
    ## SPSS5-SPSS8    0.605744018  0.631474787 0.8386089 0.8315435 1.423733e-08
    ## SPSS5-SPSS9    0.619221191  0.642141351 0.8064099 0.8016379 4.403145e-13
    ## SPSS5-SPSS10  -0.060474488 -0.032895802 0.3136078 0.3147487 1.815893e-01
    ## SPSS6-SPSS7    0.331742750  0.348098598 0.5954962 0.5839777 5.170495e-08
    ## SPSS6-SPSS8    0.488003199  0.485064685 0.7186286 0.7227732 7.719780e-11
    ## SPSS6-SPSS9    0.516813949  0.537782086 0.7263604 0.7186939 1.157519e-12
    ## SPSS6-SPSS10  -0.133533651 -0.112137463 0.2295530 0.2142264 5.969488e-01
    ## SPSS7-SPSS8    0.466847527  0.482245025 0.7025247 0.7010371 1.610592e-10
    ## SPSS7-SPSS9    0.434915945  0.445548272 0.7005403 0.6821480 1.347074e-08
    ## SPSS7-SPSS10  -0.197689451 -0.202532614 0.2231107 0.2116349 9.028841e-01
    ## SPSS8-SPSS9    0.721015071  0.728336386 0.8694364 0.8643307 5.484502e-14
    ## SPSS8-SPSS10   0.035973397  0.083158016 0.4160708 0.3930814 2.360287e-02
    ## SPSS9-SPSS10  -0.090596260 -0.068037623 0.3090340 0.2997900 2.768779e-01

## MAP Test & Parallel Analysis (PA)

``` r
# MAP Test/Parallel Analysis ----------------------------------------------
```

**MAP Test**

``` r
VSS(spss.data, fm = 'minres', cor = 'poly', plot = F)
```

    ## 
    ## Very Simple Structure
    ## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
    ##     n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
    ## VSS complexity 1 achieves a maximimum of 0.87  with  1  factors
    ## VSS complexity 2 achieves a maximimum of 0.95  with  2  factors
    ## 
    ## The Velicer MAP achieves a minimum of 0.05  with  2  factors 
    ## BIC achieves a minimum of  NA  with  2  factors
    ## Sample Size adjusted BIC achieves a minimum of  NA  with  4  factors
    ## 
    ## Statistics by number of factors 
    ##   vss1 vss2   map dof   chisq    prob sqresid  fit RMSEA BIC SABIC complex
    ## 1 0.87 0.00 0.057  35 2.3e+02 2.6e-31    4.48 0.87 0.179  53 163.7     1.0
    ## 2 0.86 0.95 0.049  26 8.7e+01 1.7e-08    1.57 0.95 0.115 -48  34.6     1.1
    ## 3 0.86 0.95 0.072  18 5.4e+01 1.9e-05    1.27 0.96 0.106 -39  17.6     1.2
    ## 4 0.86 0.94 0.116  11 1.6e+01 1.3e-01    1.17 0.97 0.052 -41  -5.8     1.3
    ## 5 0.86 0.89 0.165   5 4.6e+00 4.7e-01    0.98 0.97 0.000 -21  -5.5     1.4
    ## 6 0.85 0.91 0.240   0 7.6e-01      NA    0.90 0.97    NA  NA    NA     1.5
    ## 7 0.86 0.93 0.410  -4 5.1e-07      NA    0.90 0.97    NA  NA    NA     1.4
    ## 8 0.86 0.95 0.480  -7 1.5e-12      NA    0.85 0.98    NA  NA    NA     1.4
    ##    eChisq    SRMR eCRMS eBIC
    ## 1 2.4e+02 1.2e-01 0.138   57
    ## 2 1.6e+01 3.2e-02 0.042 -119
    ## 3 6.6e+00 2.0e-02 0.032  -87
    ## 4 2.7e+00 1.3e-02 0.026  -54
    ## 5 4.8e-01 5.5e-03 0.016  -25
    ## 6 6.1e-02 2.0e-03    NA   NA
    ## 7 1.0e-07 2.5e-06    NA   NA
    ## 8 1.3e-13 2.9e-09    NA   NA

**Parallel Analysis**

``` r
fa.parallel(spss.data, fm = 'minres', cor = 'poly', fa ='both', n.iter=100)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  2  and the number of components =  2

Both MAP and PA suggest 2F. PA should be interpreted w caution for
polychoric correlations  
Next, running 1F, 2F and 3F model. i.e., 1 above and 1 below suggested
num. of factors to help determine which model fits best

## EFA 1 Factor

``` r
# EFA 1 Factor ------------------------------------------------------------
efa1 <- fa(r = spss.data, fm = 'minres', rotate = "oblimin", cor = 'poly', nfactors = 1)
```

**EFA 1 Results**

``` r
# Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 1, rotate = "oblimin", fm = "minres", 
#          cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
#           MR1    h2   u2 com
# SPSS1E  0.89 0.791 0.21   1
# SPSS2E  0.45 0.203 0.80   1   *
# SPSS3E  0.44 0.197 0.80   1   *
# SPSS4E  0.89 0.801 0.20   1
# SPSS5E  0.80 0.642 0.36   1
# SPSS6E  0.66 0.429 0.57   1
# SPSS7E  0.63 0.393 0.61   1
# SPSS8E  0.88 0.768 0.23   1
# SPSS9E  0.90 0.803 0.20   1
# SPSS10E 0.27 0.071 0.93   1   **
# 
#                 MR1
# SS loadings    5.10
# Proportion Var 0.51
# 
# Mean item complexity =  1
# Test of the hypothesis that 1 factor is sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.36 with Chi Square of  1272.53
# The degrees of freedom for the model are 35  and the objective function was  1.36 
# 
# The root mean square of the residuals (RMSR) is  0.12 
# The df corrected root mean square of the residuals is  0.14 
# 
# The harmonic number of observations is  178 with the empirical chi square  238.29  with prob <  4.4e-32 
# The total number of observations was  178  with Likelihood Chi Square =  234.19  with prob <  2.6e-31 
# 
# Tucker Lewis Index of factoring reliability =  0.791
# RMSEA index =  0.179  and the 90 % confidence intervals are  0.158 0.201
# BIC =  52.83
# Fit based upon off diagonal values = 0.94
# Measures of factor score adequacy             
#                                                   MR1
# Correlation of (regression) scores with factors   0.97
# Multiple R square of scores with factors          0.95
# Minimum correlation of possible factor scores     0.90
```

**EFA Interpretation**  
\* RMSR = 0.12 = BAD  
\* Prop. var explained = 0.51  
\* SPSS10E = BAD factor loading (\<.4) and communality (0.07)  
\* SPSS2E and SPSS3E factor loading almost \<.4 and communality almost
\<.2  
\* 1 factor model is BAD

**EFA Outliers Check**

``` r
fS1 <- forward.search(spss.data, 1, criteria = c("mah", "GOF"))
gcdresult1 <- gCD(spss.data, 1)
ldresults1 <- LD(spss.data, 1)
```

``` r
plot(gcdresult1)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
plot(fS1)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r
plot(ldresults1)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

## EFA 2 Factors

``` r
# EFA 2 Factors -----------------------------------------------------------
efa2 <- fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 2)
```

**EFA 2 Results**

``` r
# Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 2, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
#           MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.08 0.78 0.22 1.0
# SPSS2E   0.05  0.79 0.66 0.34 1.0
# SPSS3E   0.02  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.04 0.80 0.20 1.0
# SPSS5E   0.82 -0.02 0.66 0.34 1.0
# SPSS6E   0.69 -0.04 0.45 0.55 1.0
# SPSS7E   0.71 -0.12 0.44 0.56 1.1
# SPSS8E   0.86  0.04 0.77 0.23 1.0
# SPSS9E   0.93 -0.03 0.83 0.17 1.0
# SPSS10E -0.09  0.66 0.39 0.61 1.0
# 
#                       MR1  MR2
# SS loadings           4.74 1.75
# Proportion Var        0.47 0.17 PROP EXPLAINED PER FACTOR
# Cumulative Var        0.47 0.65 *
# Proportion Explained  0.73 0.27
# Cumulative Proportion 0.73 1.00
# 
# With factor correlations of 
#       R1  MR2
# MR1 1.00 0.42
# MR2 0.42 1.00
# 
# Mean item complexity =  1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.36 with Chi Square of  1272.53
# The degrees of freedom for the model are 26  and the objective function was  0.51 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  178 with the empirical chi square  16.17  with prob <  0.93 
# The total number of observations was  178  with Likelihood Chi Square =  87  with prob <  1.7e-08 
# 
# Tucker Lewis Index of factoring reliability =  0.913
# RMSEA index =  0.115  and the 90 % confidence intervals are  0.089 0.142
# BIC =  -47.72
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
#                                                   MR1  MR2
# Correlation of (regression) scores with factors   0.98 0.91
# Multiple R square of scores with factors          0.95 0.84
# Minimum correlation of possible factor scores     0.90 0.67
```

**EFA 2 Interpretation**  
\* RMSR = 0.03 = WOW\! huge decrease by adding just 1 more factor  
\* Prop. var explained = 0.65, 14% raw difference from 1F model  
\* No poor factor loadings or low communalities  
\* Column and row parsimony is pretty amazing  
\* Notice that all negatively worded items load onto factor 2 & all
positively worded items load onto factor 1  
\* 2F prob wins, but let’s try 3F next anyways

**EFA 2 Outliers Check**

``` r
fS2 <- forward.search(spss.data, 2, criteria = c("mah", "GOF"))
gcdresult2 <- gCD(spss.data, 2)
ldresults2 <- LD(spss.data, 2)
```

``` r
plot(gcdresult2)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
plot(fS2)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

``` r
plot(ldresults2)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

## EFA 3 Factors

``` r
# EFA 3 Factors -----------------------------------------------------------
efa3 <- fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 3)
```

**EFA 3 Results**

``` r
# Factor Analysis using method =  minres
# Call: fa(r = spss.data, nfactors = 3, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
#           MR1   MR2   MR3   h2    u2 com
# SPSS1E   0.88  0.02  0.11 0.81 0.190 1.0
# SPSS2E  -0.01  0.98 -0.09 0.94 0.064 1.0
# SPSS3E   0.12  0.65  0.23 0.60 0.401 1.3
# SPSS4E   0.93 -0.04  0.15 0.85 0.145 1.1
# SPSS5E   0.78  0.07 -0.17 0.68 0.322 1.1
# SPSS6E   0.64  0.07 -0.22 0.49 0.513 1.3
# SPSS7E   0.65 -0.01 -0.22 0.47 0.531 1.2
# SPSS8E   0.86  0.04  0.00 0.77 0.231 1.0
# SPSS9E   0.93 -0.03 -0.01 0.83 0.165 1.0
# SPSS10E  0.01  0.48  0.41 0.47 0.532 1.9
# 
# MR1  MR2  MR3
# SS loadings           4.75 1.73 0.43
# Proportion Var        0.47 0.17 0.04
# Cumulative Var        0.47 0.65 0.69  *
# Proportion Explained  0.69 0.25 0.06
# Cumulative Proportion 0.69 0.94 1.00
# 
# With factor correlations of 
#     MR1  MR2  MR3
# MR1 1.00 0.39 0.01
# MR2 0.39 1.00 0.16
# MR3 0.01 0.16 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.36 with Chi Square of  1272.53
# The degrees of freedom for the model are 18  and the objective function was  0.32 
# 
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.03 
# 
# The harmonic number of observations is  178 with the empirical chi square  6.59  with prob <  0.99 
# The total number of observations was  178  with Likelihood Chi Square =  53.87  with prob <  1.9e-05 
# 
# Tucker Lewis Index of factoring reliability =  0.926
# RMSEA index =  0.106  and the 90 % confidence intervals are  0.074 0.139
# BIC =  -39.4
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# MR1  MR2  MR3
# Correlation of (regression) scores with factors   0.98 0.97 0.72
# Multiple R square of scores with factors          0.96 0.95 0.51
# Minimum correlation of possible factor scores     0.91 0.89 0.03
```

**EFA 3 Interpretation**  
\* RMSR = 0.02 = MEH only decreased by 0.01 after adding an additional
factor - not worth it bc RMSR always decreases when adding an additional
factor.  
\* Prop. var explained = 0.69, 4% raw difference from 2F model  
\* No low communalities, BUT  
\* In general, column and row parsimony is not nearly as good as 2F
model  
\* Concluding that 2F wins bc improvements in model fit isn’t worth it.
Additionally, column and row parsimony for 3F is worse than 2F model

**EFA 3 Outliers Check**

``` r
fS3 <- forward.search(spss.data, 3, criteria = c("mah", "GOF"))
gcdresult3 <- gCD(spss.data, 3)
ldresults3 <- LD(spss.data, 3)
```

``` r
plot(gcdresult3)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
plot(fS3)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

``` r
plot(ldresults3)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

## Likelihood Ratio Test (LRT) for EFA 2 vs. EFA 3

``` r
# Likelihood Ratio Test for EFA 2 vs. EFA 3 -------------------------------
anova(efa2, efa3)
```

    ## Model 1 = fa(r = spss.data, nfactors = 2, fm = "minres", cor = "poly")
    ## Model 2 = fa(r = spss.data, nfactors = 3, fm = "minres", cor = "poly")

    ## ANOVA Test for Difference Between Models
    ## 
    ##   df d.df chiSq d.chiSq PR test empirical d.empirical test.echi    BIC d.BIC
    ## 1 26      87.00                     16.17                       -47.72      
    ## 2 18    8 53.87   33.13  0 4.14      6.59        9.59       1.2 -39.40  8.32

**LRT Interpretation**

``` r
# Lower BIC indicates better fit, therefore, model 1 (i.e., 2-factor EFA) has a better fit than model 2 (3-factor EFA)  
```

## Bifactor Model

``` r
# Bifactor Model Attempt --------------------------------------------------
```

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
    ## SPSS1E  0.61  0.64       0.78 0.22 0.47
    ## SPSS2E  0.54        0.60 0.66 0.34 0.45
    ## SPSS3E  0.55        0.62 0.69 0.31 0.44
    ## SPSS4E  0.60  0.67       0.80 0.20 0.45
    ## SPSS5E  0.52  0.62       0.66 0.34 0.41
    ## SPSS6E  0.42  0.52       0.45 0.55 0.39
    ## SPSS7E  0.38  0.54       0.44 0.56 0.33
    ## SPSS8E  0.59  0.65       0.77 0.23 0.45
    ## SPSS9E  0.58  0.70       0.83 0.17 0.41
    ## SPSS10E 0.37        0.50 0.39 0.61 0.35
    ## 
    ## With eigenvalues of:
    ##   g F1* F2* 
    ## 2.7 2.7 1.0 
    ## 
    ## general/max  1   max/min =   2.7
    ## mean percent general =  0.42    with sd =  0.05 and cv of  0.11 
    ## 
    ##  The orthogonal loadings were 
    ## Unstandardized loadings based upon covariance matrix
    ##           F1   F2   h2   u2   H2   U2
    ## SPSS1E  0.85 0.26 0.78 0.22 0.78 0.22
    ## SPSS2E  0.22 0.78 0.66 0.34 0.66 0.34
    ## SPSS3E  0.20 0.81 0.69 0.31 0.69 0.31
    ## SPSS4E  0.87 0.23 0.80 0.20 0.80 0.20
    ## SPSS5E  0.80 0.16 0.66 0.34 0.66 0.34
    ## SPSS6E  0.66 0.10 0.45 0.55 0.45 0.55
    ## SPSS7E  0.66 0.03 0.44 0.56 0.44 0.56
    ## SPSS8E  0.85 0.22 0.77 0.23 0.77 0.23
    ## SPSS9E  0.90 0.16 0.83 0.17 0.83 0.17
    ## SPSS10E 0.06 0.62 0.39 0.61 0.39 0.61
    ## 
    ##                  F1   F2
    ## SS loadings    4.61 1.88
    ## Proportion Var 0.46 0.19
    ## Cumulative Var 0.46 0.65
    ## 
    ## The degrees of freedom are 26  and the fit is  0.51 
    ## 
    ## The root mean square of the residuals is  0.03 
    ## The df corrected root mean square of the residuals is  0.04

As indicated by the warning, at least three specific factors required
for identification (in an EFA context).  
As a result, the 2 Factor model still fits the data best.

## Reliability Analyses

``` r
# Reliability -------------------------------------------------------------
```

Since the 2 Factor model is multidimensional, we calculated reliability
for each factor.  
We use psych::omega() as recommended by Flora (2020)

### **Reliability Calculated Per Factor**

``` r
# split dataset into each factor
spss.data.f1 <- spss.data %>% select(-c(SPSS2E, SPSS3E, SPSS10E)) 
spss.data.f2 <- spss.data %>% select(c(SPSS2E, SPSS3E, SPSS10E)) 

# poly = TRUE because we want to use the polychoric correlation matrix instead of Pearson because of our categorical data
# since nfactors = 1, only omega total is meaningful 
omega(m = spss.data.f1, poly = TRUE, plot = F, nfactors = 1) # Omega Total 0.93   
```

    ## Omega_h for 1 factor is not meaningful, just omega_t

    ## Warning in schmid(m, nfactors, fm, digits, rotate = rotate, n.obs = n.obs, :
    ## Omega_h and Omega_asymptotic are not meaningful with one factor

    ## Omega 
    ## Call: omegah(m = m, nfactors = nfactors, fm = fm, key = key, flip = flip, 
    ##     digits = digits, title = title, sl = sl, labels = labels, 
    ##     plot = plot, n.obs = n.obs, rotate = rotate, Phi = Phi, option = option, 
    ##     covar = covar)
    ## Alpha:                 0.93 
    ## G.6:                   0.93 
    ## Omega Hierarchical:    0.93 
    ## Omega H asymptotic:    1 
    ## Omega Total            0.93 
    ## 
    ## Schmid Leiman Factor loadings greater than  0.2 
    ##           g  F1*   h2   u2 p2
    ## SPSS1E 0.88      0.78 0.22  1
    ## SPSS4E 0.90      0.81 0.19  1
    ## SPSS5E 0.81      0.66 0.34  1
    ## SPSS6E 0.67      0.44 0.56  1
    ## SPSS7E 0.65      0.42 0.58  1
    ## SPSS8E 0.88      0.77 0.23  1
    ## SPSS9E 0.91      0.83 0.17  1
    ## 
    ## With eigenvalues of:
    ##   g F1* 
    ## 4.7 0.0 
    ## 
    ## general/max  8.504964e+16   max/min =   1
    ## mean percent general =  1    with sd =  0 and cv of  0 
    ## Explained Common Variance of the general factor =  1 
    ## 
    ## The degrees of freedom are 14  and the fit is  0.24 
    ## The number of observations was  178  with Chi Square =  40.74  with prob <  2e-04
    ## The root mean square of the residuals is  0.03 
    ## The df corrected root mean square of the residuals is  0.03
    ## RMSEA index =  0.103  and the 10 % confidence intervals are  0.068 0.142
    ## BIC =  -31.81
    ## 
    ## Compare this with the adequacy of just a general factor and no group factors
    ## The degrees of freedom for just the general factor are 14  and the fit is  0.24 
    ## The number of observations was  178  with Chi Square =  40.74  with prob <  2e-04
    ## The root mean square of the residuals is  0.03 
    ## The df corrected root mean square of the residuals is  0.03 
    ## 
    ## RMSEA index =  0.103  and the 10 % confidence intervals are  0.068 0.142
    ## BIC =  -31.81 
    ## 
    ## Measures of factor score adequacy             
    ##                                                  g F1*
    ## Correlation of scores with factors            0.97   0
    ## Multiple R square of scores with factors      0.95   0
    ## Minimum correlation of factor score estimates 0.90  -1
    ## 
    ##  Total, General and Subset omega for each subset
    ##                                                  g  F1*
    ## Omega total for total scores and subscales    0.93 0.93
    ## Omega general for total scores and subscales  0.93 0.93
    ## Omega group for total scores and subscales    0.00 0.00

``` r
omega(m = spss.data.f2, poly = TRUE, plot = F, nfactors = 1) # Omega Total 0.8   
```

    ## Omega_h for 1 factor is not meaningful, just omega_t

    ## Warning in schmid(m, nfactors, fm, digits, rotate = rotate, n.obs = n.obs, :
    ## Omega_h and Omega_asymptotic are not meaningful with one factor

    ## Warning in cov2cor(t(w) %*% r %*% w): diag(.) had 0 or NA entries; non-finite
    ## result is doubtful

    ## Omega 
    ## Call: omegah(m = m, nfactors = nfactors, fm = fm, key = key, flip = flip, 
    ##     digits = digits, title = title, sl = sl, labels = labels, 
    ##     plot = plot, n.obs = n.obs, rotate = rotate, Phi = Phi, option = option, 
    ##     covar = covar)
    ## Alpha:                 0.79 
    ## G.6:                   0.73 
    ## Omega Hierarchical:    0.8 
    ## Omega H asymptotic:    1 
    ## Omega Total            0.8 
    ## 
    ## Schmid Leiman Factor loadings greater than  0.2 
    ##            g  F1*   h2   u2 p2
    ## SPSS2E  0.83      0.70 0.30  1
    ## SPSS3E  0.83      0.68 0.32  1
    ## SPSS10E 0.60      0.36 0.64  1
    ## 
    ## With eigenvalues of:
    ##   g F1* 
    ## 1.7 0.0 
    ## 
    ## general/max  Inf   max/min =   NaN
    ## mean percent general =  1    with sd =  0 and cv of  0 
    ## Explained Common Variance of the general factor =  1 
    ## 
    ## The degrees of freedom are 0  and the fit is  0 
    ## The number of observations was  178  with Chi Square =  0  with prob <  NA
    ## The root mean square of the residuals is  0 
    ## The df corrected root mean square of the residuals is  NA
    ## 
    ## Compare this with the adequacy of just a general factor and no group factors
    ## The degrees of freedom for just the general factor are 0  and the fit is  0 
    ## The number of observations was  178  with Chi Square =  0  with prob <  NA
    ## The root mean square of the residuals is  0 
    ## The df corrected root mean square of the residuals is  NA 
    ## 
    ## Measures of factor score adequacy             
    ##                                                  g F1*
    ## Correlation of scores with factors            0.91   0
    ## Multiple R square of scores with factors      0.83   0
    ## Minimum correlation of factor score estimates 0.67  -1
    ## 
    ##  Total, General and Subset omega for each subset
    ##                                                 g F1*
    ## Omega total for total scores and subscales    0.8 0.8
    ## Omega general for total scores and subscales  0.8 0.8
    ## Omega group for total scores and subscales    0.0 0.0

``` r
# warning message regarding 'non-finite result is doubtful' refers to the NA or NaN values in the output. They should not be trusted, but exist because the input provided has NA values
```

### **Reliability Calculated for Overall Scale**

The following code calculates omega for the scale overall (i.e.,
treating it as unidimensional).

``` r
omega(m = spss.data, poly = TRUE, plot = F, nfactors = 2) # Omega Total for total scores = 0.93, for F1 = 0.94 and for F2 = 0.80   
```

    ## 
    ## Three factors are required for identification -- general factor loadings set to be equal. 
    ## Proceed with caution. 
    ## Think about redoing the analysis with alternative values of the 'option' setting.

    ## Omega 
    ## Call: omegah(m = m, nfactors = nfactors, fm = fm, key = key, flip = flip, 
    ##     digits = digits, title = title, sl = sl, labels = labels, 
    ##     plot = plot, n.obs = n.obs, rotate = rotate, Phi = Phi, option = option, 
    ##     covar = covar)
    ## Alpha:                 0.9 
    ## G.6:                   0.93 
    ## Omega Hierarchical:    0.52 
    ## Omega H asymptotic:    0.55 
    ## Omega Total            0.93 
    ## 
    ## Schmid Leiman Factor loadings greater than  0.2 
    ##            g   F1*   F2*   h2   u2   p2
    ## SPSS1E  0.61  0.64       0.78 0.22 0.47
    ## SPSS2E  0.54        0.60 0.66 0.34 0.45
    ## SPSS3E  0.55        0.62 0.69 0.31 0.44
    ## SPSS4E  0.60  0.67       0.80 0.20 0.45
    ## SPSS5E  0.52  0.62       0.66 0.34 0.41
    ## SPSS6E  0.42  0.52       0.45 0.55 0.39
    ## SPSS7E  0.38  0.54       0.44 0.56 0.33
    ## SPSS8E  0.59  0.65       0.77 0.23 0.45
    ## SPSS9E  0.58  0.70       0.83 0.17 0.41
    ## SPSS10E 0.37        0.50 0.39 0.61 0.35
    ## 
    ## With eigenvalues of:
    ##   g F1* F2* 
    ## 2.7 2.7 1.0 
    ## 
    ## general/max  1   max/min =   2.7
    ## mean percent general =  0.42    with sd =  0.05 and cv of  0.11 
    ## Explained Common Variance of the general factor =  0.42 
    ## 
    ## The degrees of freedom are 26  and the fit is  0.51 
    ## The number of observations was  178  with Chi Square =  87  with prob <  1.7e-08
    ## The root mean square of the residuals is  0.03 
    ## The df corrected root mean square of the residuals is  0.04
    ## RMSEA index =  0.115  and the 10 % confidence intervals are  0.089 0.142
    ## BIC =  -47.72
    ## 
    ## Compare this with the adequacy of just a general factor and no group factors
    ## The degrees of freedom for just the general factor are 35  and the fit is  3.53 
    ## The number of observations was  178  with Chi Square =  608.32  with prob <  9.3e-106
    ## The root mean square of the residuals is  0.28 
    ## The df corrected root mean square of the residuals is  0.32 
    ## 
    ## RMSEA index =  0.303  and the 10 % confidence intervals are  0.283 0.326
    ## BIC =  426.95 
    ## 
    ## Measures of factor score adequacy             
    ##                                                  g  F1*  F2*
    ## Correlation of scores with factors            0.74 0.80 0.74
    ## Multiple R square of scores with factors      0.55 0.64 0.55
    ## Minimum correlation of factor score estimates 0.10 0.27 0.11
    ## 
    ##  Total, General and Subset omega for each subset
    ##                                                  g  F1*  F2*
    ## Omega total for total scores and subscales    0.93 0.94 0.80
    ## Omega general for total scores and subscales  0.52 0.39 0.34
    ## Omega group for total scores and subscales    0.42 0.54 0.47

## Convergent Validity Testing with Quantitative Attitudes Scale

``` r
# Convergent Validity -----------------------------------------------------
```

The following section runs Pearson Correlations between ATSPSS scale and
Quantitative Attitudes scale  
to test for convergent validity.  
First, the Quantitative Attitudes scale needs to be setup properly
(e.g., reverse code items).  
**Quantitative Attitudes Setup**

``` r
qa.data <- full.data %>% select(
  MA1E:MA8E
) %>% select(
  -MA6E 
  # removed the item: 'Statistics is a not a worthwhile or necessary subject' based on previous validation paper (Kunicki et al., 2020)
) %>% mutate(
  MA2E = car::recode(MA2E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1"), 
  # reverse code 'Math is one of my most dreaded subjects'
  MA3E = car::recode(MA3E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1"), 
  # reverse code 'I have seldom liked studying mathematics'
  MA7E = car::recode(MA7E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1") 
  # reverse code 'I am not willing to take more than the required amount of statistics courses'
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Quantitative Attitudes Factor 1
qa.data.f1 <- qa.data %>% select(
  MA1E:MA4E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Quantitative Attitudes Factor 2
qa.data.f2 <- qa.data %>% select(
  MA5E, MA7E, MA8E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
```

**ATSPSS Setup**  
Similarly, the ATSPSS needs to be setup by splitting into its two
factors

``` r
# SPSS Attitudes Factor 1
sa.data.f1 <- spss.data.f1 %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# SPSS Attitudes Factor 2
sa.data.f2 <- spss.data.f2 %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
```

Ok, now we’re ready to run the correlations (and scatterplots) for
convergent validity testing.

**Correlations**

``` r
cor.test(qa.data.f1$total, sa.data.f1$total) # r = 0.1528329 95% [0.005880347 0.293323814] t = 2.0517, df = 176, p-value = 0.04168  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f1$total and sa.data.f1$total
    ## t = 2.0517, df = 176, p-value = 0.04168
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.005880347 0.293323814
    ## sample estimates:
    ##       cor 
    ## 0.1528329

``` r
cor.test(qa.data.f1$total, sa.data.f2$total) # r = 0.1854679 95% [0.03945967 0.32372166] t = 2.504, df = 176, p-value = 0.01319  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f1$total and sa.data.f2$total
    ## t = 2.504, df = 176, p-value = 0.01319
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.03945967 0.32372166
    ## sample estimates:
    ##       cor 
    ## 0.1854679

``` r
car::scatterplot(qa.data.f1$total, sa.data.f1$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
car::scatterplot(qa.data.f1$total, sa.data.f2$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-43-2.png)<!-- -->

``` r
cor.test(qa.data.f2$total, sa.data.f1$total) # r = 0.247044 95% [0.1037284 0.3803096] t = 3.3822, df = 176, p-value = 0.000886  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f2$total and sa.data.f1$total
    ## t = 3.3822, df = 176, p-value = 0.000886
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1037284 0.3803096
    ## sample estimates:
    ##      cor 
    ## 0.247044

``` r
cor.test(qa.data.f2$total, sa.data.f2$total) # r = 0.1250142 95% [-0.02248388  0.26718600] t = 1.6716, df = 176, p-value = 0.09638  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f2$total and sa.data.f2$total
    ## t = 1.6716, df = 176, p-value = 0.09638
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.02248388  0.26718600
    ## sample estimates:
    ##       cor 
    ## 0.1250142

``` r
car::scatterplot(qa.data.f2$total, sa.data.f1$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-43-3.png)<!-- -->

``` r
car::scatterplot(qa.data.f2$total, sa.data.f2$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-43-4.png)<!-- -->

The following section runs Pearson Correlations between ATSPSS scale  
and Quantitative Anxiety scale / Quantitative Hindrances scale

**Quantitative Anxiety**

``` r
# Set up Quant. Anxiety scale by selecting only the items and calculating total score
qanx.data <- full.data %>% select(
  QANX1E:QANX4E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)

# Ok, now for the correlations  
cor.test(qanx.data$total, sa.data.f1$total) # r = -0.06134227 95% [-0.20656324  0.08652308] t = -0.81533, df = 176, p-value = 0.416  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qanx.data$total and sa.data.f1$total
    ## t = -0.81533, df = 176, p-value = 0.416
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.20656324  0.08652308
    ## sample estimates:
    ##         cor 
    ## -0.06134227

``` r
cor.test(qanx.data$total, sa.data.f2$total) # r = -0.07568429 95% [-0.22031643  0.07220418] t = -1.007, df = 176, p-value = 0.3153  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qanx.data$total and sa.data.f2$total
    ## t = -1.007, df = 176, p-value = 0.3153
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.22031643  0.07220418
    ## sample estimates:
    ##         cor 
    ## -0.07568429

``` r
car::scatterplot(qanx.data$total, sa.data.f1$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
car::scatterplot(qanx.data$total, sa.data.f2$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-44-2.png)<!-- -->

**Quantitative Hindrances**  
Again, first set up Quant. Hindrances scale by selecting only the items
and calculating total score

``` r
qh.data <- full.data %>% select(
  QHIND1E:QHIND5E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Now for the correlations  
cor.test(qh.data$total, sa.data.f1$total) # r = -0.03837761 95% [-0.1844213  0.1093242] t = -0.50951, df = 176, p-value = 0.611  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qh.data$total and sa.data.f1$total
    ## t = -0.50951, df = 176, p-value = 0.611
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1844213  0.1093242
    ## sample estimates:
    ##         cor 
    ## -0.03837761

``` r
cor.test(qh.data$total, sa.data.f2$total) # r = -0.06702523 95% [-0.21201975  0.08085658] t = -0.89119, df = 176, p-value = 0.374  
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qh.data$total and sa.data.f2$total
    ## t = -0.89119, df = 176, p-value = 0.374
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.21201975  0.08085658
    ## sample estimates:
    ##         cor 
    ## -0.06702523

``` r
car::scatterplot(qh.data$total, sa.data.f1$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
car::scatterplot(qh.data$total, sa.data.f2$total)
```

![](ATSPSSOpenCode_files/figure-gfm/unnamed-chunk-45-2.png)<!-- -->

## Correlations between All Measures

``` r
# Correlations between all Measures ---------------------------------------
```

The following correlations are to fill out the remaining cells of the
convergent validity table.

**ATSPSS Factor 1 x ATSPSS Factor 2**

``` r
cor.test(sa.data.f1$total, sa.data.f2$total) # r = 0.2943086 95% [0.1538854 0.4230790]  t = 4.0854, df = 176, p-value = 6.675e-05
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  sa.data.f1$total and sa.data.f2$total
    ## t = 4.0854, df = 176, p-value = 6.675e-05
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1538854 0.4230790
    ## sample estimates:
    ##       cor 
    ## 0.2943086

**Quant. Attitudes Factor 1 x Quant. Attitudes Factor 2**

``` r
cor.test(qa.data.f1$total, qa.data.f2$total) # r = 0.4117248 95% [0.2816993 0.5269012] t = 5.9937, df = 176, p-value = 1.132e-08
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f1$total and qa.data.f2$total
    ## t = 5.9937, df = 176, p-value = 1.132e-08
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2816993 0.5269012
    ## sample estimates:
    ##       cor 
    ## 0.4117248

**Quant. Attitudes Factor 1 x Quant. Anxiety**

``` r
cor.test(qa.data.f1$total, qanx.data$total) # r = -0.5866839 95% [ -0.6754799 -0.4811158] t = -9.6111, df = 176, p-value < 2.2e-16
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f1$total and qanx.data$total
    ## t = -9.6111, df = 176, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.6754799 -0.4811158
    ## sample estimates:
    ##        cor 
    ## -0.5866839

**Quant. Attitudes Factor 1 x Quant. Hindrances**

``` r
cor.test(qa.data.f1$total, qh.data$total) # r = -0.4092008 95% [-0.5247049 -0.2789024] t = -5.9496, df = 176, p-value = 1.418e-08
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f1$total and qh.data$total
    ## t = -5.9496, df = 176, p-value = 1.418e-08
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5247049 -0.2789024
    ## sample estimates:
    ##        cor 
    ## -0.4092008

**Quant. Attitudes Factor 2 x Quant. Anxiety**

``` r
cor.test(qa.data.f2$total, qanx.data$total) # r = -0.3704627 95% [-0.4908038 -0.2362512] t = -5.2912, df = 176, p-value = 3.58e-07
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f2$total and qanx.data$total
    ## t = -5.2912, df = 176, p-value = 3.58e-07
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4908038 -0.2362512
    ## sample estimates:
    ##        cor 
    ## -0.3704627

**Quant. Attitudes Factor 2 x Quant. Hindrances**

``` r
cor.test(qa.data.f2$total, qh.data$total) # r = -0.2618719 95% [-0.3937889 -0.1193856] t = -3.5997, df = 176, p-value = 0.000414
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qa.data.f2$total and qh.data$total
    ## t = -3.5997, df = 176, p-value = 0.000414
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3937889 -0.1193856
    ## sample estimates:
    ##        cor 
    ## -0.2618719

**Quant. Anxiety x Quant. Hindrances**

``` r
cor.test(qanx.data$total, qh.data$total) # r = 0.6228247 95% [0.5237166 0.7052984] t = 10.561, df = 176, p-value < 2.2e-16
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  qanx.data$total and qh.data$total
    ## t = 10.561, df = 176, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5237166 0.7052984
    ## sample estimates:
    ##       cor 
    ## 0.6228247

## Confirmatory Multidimensional Item Response Theory (MIRT)

``` r
# Confirmatory Multidimensional Item Response Theory (MIRT) ---------------
```

``` r
irtmodel <- " 
F1 = SPSS1E, SPSS4E, SPSS5E, SPSS6E, SPSS7E, SPSS8E, SPSS9E
F2 = SPSS2E, SPSS3E, SPSS10E
COV = F1*F2" #asterisk used to call for covariance
cmirtmod <- mirt.model(irtmodel, itemnames = spss.data) 
cmirt <- mirt(data = spss.data, model = cmirtmod, itemtype = 'graded')
```

``` r
coef(cmirt , IRTParam = T, simplify = T)
```

    ## $items
    ##            a1    a2    d1    d2     d3     d4
    ## SPSS1E  3.997 0.000 6.957 4.064  0.530 -4.052
    ## SPSS2E  0.000 3.086 6.079 3.087  1.447 -2.197
    ## SPSS3E  0.000 3.066 5.328 3.155  0.960 -3.106
    ## SPSS4E  4.467 0.000 8.295 3.983  0.186 -5.773
    ## SPSS5E  2.774 0.000 5.857 3.631  0.793 -1.996
    ## SPSS6E  1.639 0.000 3.135 2.038  0.207 -1.466
    ## SPSS7E  1.588 0.000 3.655 1.407 -0.080 -1.945
    ## SPSS8E  3.442 0.000 7.028 4.088  0.684 -3.426
    ## SPSS9E  4.560 0.000 8.014 4.228 -0.078 -4.656
    ## SPSS10E 0.000 1.573 3.648 1.811  0.212 -2.034
    ## 
    ## $means
    ## F1 F2 
    ##  0  0 
    ## 
    ## $cov
    ##     F1  F2
    ## F1 1.0 0.5
    ## F2 0.5 1.0

**MIRT Model Fit**

``` r
M2(cmirt, type = "C2")
```

    ##             M2 df           p      RMSEA   RMSEA_5   RMSEA_95      SRMSR
    ## stats 57.45641 34 0.007196968 0.06243163 0.0324369 0.08944299 0.08820926
    ##             TLI       CFI
    ## stats 0.9811121 0.9857292

**MIRT Assumptions**

``` r
residuals(cmirt)
```

    ## LD matrix (lower triangle) and standardized values:
    ## 
    ##          SPSS1E  SPSS2E SPSS3E  SPSS4E  SPSS5E SPSS6E SPSS7E  SPSS8E  SPSS9E
    ## SPSS1E       NA   0.424  0.392   0.226  -0.461 -0.199 -0.224  -0.284  -0.249
    ## SPSS2E  127.880      NA -0.424  -0.427   0.264 -0.214 -0.289  -0.323  -0.363
    ## SPSS3E  109.210 128.186     NA  -0.324  -0.279 -0.212 -0.258   0.272  -0.257
    ## SPSS4E   36.465 129.587 74.758      NA  -0.806 -0.196 -0.186  -0.309  -0.292
    ## SPSS5E  151.375  49.721 55.339 462.624      NA  0.271 -0.218  -0.252  -0.197
    ## SPSS6E   28.309  32.688 31.957  27.333  52.439     NA -0.264   0.166  -0.184
    ## SPSS7E   35.818  59.299 47.315  24.502  33.901 49.514     NA  -0.237  -0.244
    ## SPSS8E   57.491  74.379 52.782  67.942  45.266 19.632 40.000      NA  -0.258
    ## SPSS9E   44.141  93.740 46.854  60.503  27.716 24.233 42.376  47.429      NA
    ## SPSS10E 112.082 103.058 87.417 114.234 121.545 64.857 87.976 145.586 163.213
    ##         SPSS10E
    ## SPSS1E    0.397
    ## SPSS2E   -0.380
    ## SPSS3E   -0.350
    ## SPSS4E   -0.401
    ## SPSS5E   -0.413
    ## SPSS6E   -0.302
    ## SPSS7E   -0.352
    ## SPSS8E    0.452
    ## SPSS9E   -0.479
    ## SPSS10E      NA

## Sensitivity Analyses

``` r
# Sensitivity Analyses ----------------------------------------------------
```

### **Setting up Datasets**

``` r
# missing data is removed using listwise deletion
str(spss.data)
```

    ## 'data.frame':    178 obs. of  10 variables:
    ##  $ SPSS1E : num  4 4 3 2 4 2 4 2 3 4 ...
    ##  $ SPSS2E : num  4 5 3 4 5 2 3 2 4 5 ...
    ##  $ SPSS3E : num  4 5 3 4 4 2 1 2 5 5 ...
    ##  $ SPSS4E : num  3 4 2 2 3 3 2 3 3 4 ...
    ##  $ SPSS5E : num  3 4 3 2 4 2 4 3 3 3 ...
    ##  $ SPSS6E : num  3 4 2 2 4 1 4 3 1 5 ...
    ##  $ SPSS7E : num  4 4 2 2 3 3 2 3 4 4 ...
    ##  $ SPSS8E : num  4 4 3 2 4 3 3 3 3 2 ...
    ##  $ SPSS9E : num  4 4 3 2 3 3 2 2 2 4 ...
    ##  $ SPSS10E: num  4 5 3 4 5 3 3 3 4 3 ...

``` r
# missing data is not removed. This data will be used to imputing missing values later.
str(spss.data.withNA)
```

    ## tibble [181 x 10] (S3: tbl_df/tbl/data.frame)
    ##  $ SPSS1E : num [1:181] 4 4 3 2 4 2 4 2 3 4 ...
    ##  $ SPSS2E : num [1:181] 4 5 3 4 5 2 3 2 4 5 ...
    ##  $ SPSS3E : num [1:181] 4 5 3 4 4 2 1 2 5 5 ...
    ##  $ SPSS4E : num [1:181] 3 4 2 2 3 3 2 3 3 4 ...
    ##  $ SPSS5E : num [1:181] 3 4 3 2 4 2 4 3 3 3 ...
    ##  $ SPSS6E : num [1:181] 3 4 2 2 4 1 4 3 1 5 ...
    ##  $ SPSS7E : num [1:181] 4 4 2 2 3 3 2 3 4 4 ...
    ##  $ SPSS8E : num [1:181] 4 4 3 2 4 3 3 3 3 2 ...
    ##  $ SPSS9E : num [1:181] 4 4 3 2 3 3 2 2 2 4 ...
    ##  $ SPSS10E: num [1:181] 4 5 3 4 5 3 3 3 4 3 ...

### **Assumptions for Maximum Likelihood**

Refer to ‘Statistical Assumptions’ section at the beginning

``` r
# Multivariate Normality
# Linearity
```

### **Treating our 5-Point Likert Scale as Continuous instead of Categorical**

#### **First, let’s do this with the 2 factor EFA model**

##### **Using Pearson correlations, ML estimator and listwise deletion**

``` r
snstvty_efa2 <- fa(r = spss.data, fm = "ml", nfactors = 2)

# Factor Analysis using method =  ml
# Call: fa(r = spss.data, nfactors = 2, fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#           ML1   ML2   h2   u2 com
# SPSS1E   0.84  0.06 0.74 0.26   1
# SPSS2E   0.02  0.76 0.59 0.41   1
# SPSS3E   0.02  0.77 0.60 0.40   1
# SPSS4E   0.87  0.00 0.76 0.24   1
# SPSS5E   0.76 -0.01 0.58 0.42   1
# SPSS6E   0.62 -0.03 0.38 0.62   1
# SPSS7E   0.63 -0.08 0.36 0.64   1
# SPSS8E   0.81  0.04 0.68 0.32   1
# SPSS9E   0.90 -0.03 0.78 0.22   1
# SPSS10E -0.07  0.59 0.32 0.68   1
# 
#                       ML1  ML2
# SS loadings           4.29 1.51
# Proportion Var        0.43 0.15
# Cumulative Var        0.43 0.58
# Proportion Explained  0.74 0.26
# Cumulative Proportion 0.74 1.00
# 
# With factor correlations of 
#      ML1  ML2
# ML1 1.00 0.38
# ML2 0.38 1.00
# 
# Mean item complexity =  1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  5.61 with Chi Square of  969.94
# The degrees of freedom for the model are 26  and the objective function was  0.27 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  178 with the empirical chi square  15.35  with prob <  0.95 
# The total number of observations was  178  with Likelihood Chi Square =  46.19  with prob <  0.0087 
# 
# Tucker Lewis Index of factoring reliability =  0.962
# RMSEA index =  0.066  and the 90 % confidence intervals are  0.033 0.097
# BIC =  -88.53
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
#                                                   ML1  ML2
# Correlation of (regression) scores with factors   0.97 0.88
# Multiple R square of scores with factors          0.94 0.78
# Minimum correlation of possible factor scores     0.87 0.56
```

#### **OK, now let’s do the same thing, but with the 3 factor EFA model**

##### **Using Pearson correlations, ML estimator and listwise deletion**

``` r
snstvty_efa3 <- fa(r = spss.data, fm = "ml", nfactors = 3)

# Factor Analysis using method =  ml
# Call: fa(r = spss.data, nfactors = 3, fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#           ML1   ML2   ML3   h2    u2 com
# SPSS1E   0.54  0.09  0.32 0.73 0.274 1.7
# SPSS2E   0.12  0.76 -0.10 0.60 0.401 1.1
# SPSS3E  -0.03  0.76  0.06 0.60 0.402 1.0
# SPSS4E   0.06  0.02  0.94 1.00 0.005 1.0
# SPSS5E   0.68  0.01  0.11 0.59 0.408 1.1
# SPSS6E   0.68 -0.02 -0.05 0.41 0.591 1.0
# SPSS7E   0.66 -0.06 -0.02 0.39 0.609 1.0
# SPSS8E   0.89  0.06 -0.07 0.75 0.254 1.0
# SPSS9E   0.64  0.00  0.28 0.76 0.238 1.4
# SPSS10E -0.17  0.58  0.11 0.32 0.676 1.2
# 
#                       ML1  ML2  ML3
# SS loadings           3.18 1.55 1.41
# Proportion Var        0.32 0.15 0.14
# Cumulative Var        0.32 0.47 0.61
# Proportion Explained  0.52 0.25 0.23
# Cumulative Proportion 0.52 0.77 1.00
# 
# With factor correlations of 
#       ML1  ML2  ML3
# ML1 1.00 0.35 0.79
# ML2 0.35 1.00 0.31
# ML3 0.79 0.31 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  5.61 with Chi Square of  969.94
# The degrees of freedom for the model are 18  and the objective function was  0.14 
# 
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  178 with the empirical chi square  8.75  with prob <  0.97 
# The total number of observations was  178  with Likelihood Chi Square =  24.53  with prob <  0.14 
# 
# Tucker Lewis Index of factoring reliability =  0.982
# RMSEA index =  0.045  and the 90 % confidence intervals are  0 0.086
# BIC =  -68.74
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
#                                                     ML1  ML2  ML3
# Correlation of (regression) scores with factors   0.96 0.88 1.00
# Multiple R square of scores with factors          0.91 0.78 0.99
# Minimum correlation of possible factor scores     0.83 0.56 0.99
```

### **Setup: Imputations for Missing Values**

``` r
imp <- mice(spss.data.withNA, m = 20) 
imp <-  complete(imp)
sum(is.na(imp)) # double checking that there is no missing data
```

### **Treating Our Data as Categorical, but imputed missing values**

#### **First, with the 2 factor EFA model**

``` r
efa2imp <- fa(r = imp, fm = "minres", cor = 'poly', nfactors = 2)

# Factor Analysis using method =  minres
# Call: fa(r = imp, nfactors = 2, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.08 0.78 0.22 1.0
# SPSS2E   0.05  0.79 0.66 0.34 1.0
# SPSS3E   0.02  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.04 0.80 0.20 1.0
# SPSS5E   0.82 -0.02 0.66 0.34 1.0
# SPSS6E   0.68 -0.04 0.45 0.55 1.0
# SPSS7E   0.70 -0.12 0.44 0.56 1.1
# SPSS8E   0.86  0.05 0.77 0.23 1.0
# SPSS9E   0.93 -0.04 0.83 0.17 1.0
# SPSS10E -0.09  0.66 0.39 0.61 1.0
# 
# MR1  MR2
# SS loadings           4.72 1.75
# Proportion Var        0.47 0.18
# Cumulative Var        0.47 0.65
# Proportion Explained  0.73 0.27
# Cumulative Proportion 0.73 1.00
# 
# With factor correlations of 
# MR1  MR2
# MR1 1.00 0.42
# MR2 0.42 1.00
# 
# Mean item complexity =  1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.31 with Chi Square of  1285.8
# The degrees of freedom for the model are 26  and the objective function was  0.49 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  181 with the empirical chi square  16.23  with prob <  0.93 
# The total number of observations was  181  with Likelihood Chi Square =  84.77  with prob <  3.8e-08 
# 
# Tucker Lewis Index of factoring reliability =  0.917
# RMSEA index =  0.112  and the 90 % confidence intervals are  0.086 0.139
# BIC =  -50.39
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# MR1
# Correlation of (regression) scores with factors   0.98
# Multiple R square of scores with factors          0.95
# Minimum correlation of possible factor scores     0.90
# MR2
# Correlation of (regression) scores with factors   0.91
# Multiple R square of scores with factors          0.84
# Minimum correlation of possible factor scores     0.67  
```

#### **Next, with the 3 factor EFA model**

``` r
efa3imp <- fa(r = imp, fm = "minres", cor = 'poly', nfactors = 3)

# Factor Analysis using method =  minres
# Call: fa(r = imp, nfactors = 3, fm = "minres", cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# MR1   MR2   MR3   h2    u2 com
# SPSS1E   0.88  0.02  0.11 0.81 0.192 1.0
# SPSS2E  -0.01  0.98 -0.08 0.94 0.061 1.0
# SPSS3E   0.12  0.64  0.24 0.60 0.401 1.3
# SPSS4E   0.93 -0.04  0.15 0.85 0.148 1.1
# SPSS5E   0.78  0.07 -0.17 0.68 0.322 1.1
# SPSS6E   0.64  0.07 -0.21 0.48 0.519 1.3
# SPSS7E   0.65  0.00 -0.22 0.47 0.532 1.2
# SPSS8E   0.86  0.04  0.00 0.76 0.236 1.0
# SPSS9E   0.93 -0.04 -0.01 0.83 0.166 1.0
# SPSS10E  0.01  0.47  0.42 0.47 0.528 2.0
# 
# MR1  MR2  MR3
# SS loadings           4.74 1.72 0.43
# Proportion Var        0.47 0.17 0.04
# Cumulative Var        0.47 0.65 0.69
# Proportion Explained  0.69 0.25 0.06
# Cumulative Proportion 0.69 0.94 1.00
# 
# With factor correlations of 
# MR1  MR2  MR3
# MR1 1.00 0.38 0.02
# MR2 0.38 1.00 0.17
# MR3 0.02 0.17 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.31 with Chi Square of  1285.8
# The degrees of freedom for the model are 18  and the objective function was  0.3 
# 
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.03 
# 
# The harmonic number of observations is  181 with the empirical chi square  6.71  with prob <  0.99 
# The total number of observations was  181  with Likelihood Chi Square =  52.21  with prob <  3.5e-05 
# 
# Tucker Lewis Index of factoring reliability =  0.93
# RMSEA index =  0.102  and the 90 % confidence intervals are  0.071 0.136
# BIC =  -41.37
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# MR1  MR2  MR3
# Correlation of (regression) scores with factors   0.98 0.97 0.72
# Multiple R square of scores with factors          0.96 0.95 0.51
# Minimum correlation of possible factor scores     0.91 0.90 0.03
```

### **Treating Our Data as Categorical, but using pairwise deletion for missing values**

#### **First, with the 2 factor EFA model**

``` r
efa2pair <- fa(r = spss.data.withNA, use = "pairwise", fm = "minres", cor = 'poly', nfactors = 2)

# Factor Analysis using method =  minres
# Call: fa(r = spss.data.withNA, nfactors = 2, fm = "minres", use = "pairwise", 
#          cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.08 0.79 0.21 1.0
# SPSS2E   0.05  0.79 0.66 0.34 1.0
# SPSS3E   0.02  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.04 0.80 0.20 1.0
# SPSS5E   0.82 -0.02 0.66 0.34 1.0
# SPSS6E   0.68 -0.04 0.45 0.55 1.0
# SPSS7E   0.70 -0.12 0.44 0.56 1.1
# SPSS8E   0.86  0.05 0.77 0.23 1.0
# SPSS9E   0.93 -0.04 0.83 0.17 1.0
# SPSS10E -0.09  0.66 0.39 0.61 1.0
# 
# MR1  MR2
# SS loadings           4.72 1.75
# Proportion Var        0.47 0.17
# Cumulative Var        0.47 0.65
# Proportion Explained  0.73 0.27
# Cumulative Proportion 0.73 1.00
# 
# With factor correlations of 
# MR1  MR2
# MR1 1.00 0.42
# MR2 0.42 1.00
# 
# Mean item complexity =  1
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.31 with Chi Square of  1284.96
# The degrees of freedom for the model are 26  and the objective function was  0.49 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  180 with the empirical chi square  16.09  with prob <  0.93 
# The total number of observations was  181  with Likelihood Chi Square =  84.89  with prob <  3.6e-08 
# 
# Tucker Lewis Index of factoring reliability =  0.917
# RMSEA index =  0.112  and the 90 % confidence intervals are  0.086 0.139
# BIC =  -50.27
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# MR1  MR2
# Correlation of (regression) scores with factors   0.98 0.91
# Multiple R square of scores with factors          0.95 0.83
# Minimum correlation of possible factor scores     0.90 0.67
```

#### **Next, with the 3 factor EFA model**

``` r
efa3pair <- fa(r = spss.data.withNA, use = "pairwise", fm = "minres", cor = 'poly', nfactors = 3)

# Factor Analysis using method =  minres
# Call: fa(r = spss.data.withNA, nfactors = 3, fm = "minres", use = "pairwise", 
#          cor = "poly")
# Standardized loadings (pattern matrix) based upon correlation matrix
# MR1   MR2   MR3   h2    u2 com
# SPSS1E   0.88  0.02  0.11 0.81 0.191 1.0
# SPSS2E  -0.01  0.99 -0.08 0.95 0.051 1.0
# SPSS3E   0.13  0.63  0.24 0.59 0.406 1.4
# SPSS4E   0.93 -0.04  0.14 0.85 0.148 1.1
# SPSS5E   0.78  0.07 -0.17 0.67 0.325 1.1
# SPSS6E   0.64  0.07 -0.21 0.48 0.519 1.3
# SPSS7E   0.65  0.00 -0.22 0.47 0.533 1.2
# SPSS8E   0.86  0.04  0.00 0.77 0.235 1.0
# SPSS9E   0.93 -0.04 -0.01 0.83 0.166 1.0
# SPSS10E  0.02  0.46  0.43 0.47 0.526 2.0
# 
# MR1  MR2  MR3
# SS loadings           4.74 1.71 0.45
# Proportion Var        0.47 0.17 0.04
# Cumulative Var        0.47 0.65 0.69
# Proportion Explained  0.69 0.25 0.06
# Cumulative Proportion 0.69 0.94 1.00
# 
# With factor correlations of 
# MR1  MR2  MR3
# MR1 1.00 0.38 0.02
# MR2 0.38 1.00 0.19
# MR3 0.02 0.19 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  45  and the objective function was  7.31 with Chi Square of  1284.96
# The degrees of freedom for the model are 18  and the objective function was  0.3 
# 
# The root mean square of the residuals (RMSR) is  0.02 
# The df corrected root mean square of the residuals is  0.03 
# 
# The harmonic number of observations is  180 with the empirical chi square  6.6  with prob <  0.99 
# The total number of observations was  181  with Likelihood Chi Square =  52.43  with prob <  3.2e-05 
# 
# Tucker Lewis Index of factoring reliability =  0.93
# RMSEA index =  0.103  and the 90 % confidence intervals are  0.071 0.136
# BIC =  -41.15
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# MR1  MR2  MR3
# Correlation of (regression) scores with factors   0.98 0.98 0.72
# Multiple R square of scores with factors          0.96 0.96 0.52
# Minimum correlation of possible factor scores     0.91 0.92 0.03
```

### **Oblique Rotations for 2 Factor EFA**

Since we decided the 2 factor EFA fit the data best, we’re going to try
various oblique rotations  
to find the one that provides the best row and column parsimony:  
**BentlerQ**

``` r
efa2temp <- fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'bentlerQ', nfactors = 2)
#           MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.08 0.78 0.22 1.0
# SPSS2E   0.04  0.79 0.66 0.34 1.0
# SPSS3E   0.02  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.04 0.80 0.20 1.0
# SPSS5E   0.82 -0.02 0.66 0.34 1.0
# SPSS6E   0.69 -0.05 0.45 0.55 1.0
# SPSS7E   0.71 -0.12 0.44 0.56 1.1
# SPSS8E   0.86  0.04 0.77 0.23 1.0
# SPSS9E   0.93 -0.04 0.83 0.17 1.0
# SPSS10E -0.09  0.66 0.39 0.61 1.0
```

**GeominQ**

``` r
efa2temp <- fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'geominQ', nfactors = 2)
#           MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.09 0.78 0.22 1.0
# SPSS2E   0.05  0.79 0.66 0.34 1.0
# SPSS3E   0.03  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.05 0.80 0.20 1.0
# SPSS5E   0.82 -0.01 0.66 0.34 1.0
# SPSS6E   0.68 -0.04 0.45 0.55 1.0
# SPSS7E   0.70 -0.11 0.44 0.56 1.1
# SPSS8E   0.86  0.05 0.77 0.23 1.0
# SPSS9E   0.92 -0.03 0.83 0.17 1.0
# SPSS10E -0.08  0.65 0.39 0.61 1.0
```

**Quartimin**

``` r
efa2temp <- fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "quartimin", nfactors = 2)

#         MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.08 0.78 0.22 1.0
# SPSS2E   0.05  0.79 0.66 0.34 1.0
# SPSS3E   0.02  0.82 0.69 0.31 1.0
# SPSS4E   0.88  0.04 0.80 0.20 1.0
# SPSS5E   0.82 -0.02 0.66 0.34 1.0
# SPSS6E   0.69 -0.04 0.45 0.55 1.0
# SPSS7E   0.71 -0.12 0.44 0.56 1.1
# SPSS8E   0.86  0.04 0.77 0.23 1.0
# SPSS9E   0.93 -0.03 0.83 0.17 1.0
# SPSS10E -0.09  0.66 0.39 0.61 1.0
```

**Promax**

``` r
efa2temp <- fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "Promax", nfactors = 2)

# MR1   MR2   h2   u2 com
# SPSS1E   0.85  0.09 0.78 0.22   1
# SPSS2E   0.08  0.78 0.66 0.34   1
# SPSS3E   0.06  0.81 0.69 0.31   1
# SPSS4E   0.87  0.05 0.80 0.20   1
# SPSS5E   0.81  0.00 0.66 0.34   1
# SPSS6E   0.68 -0.03 0.45 0.55   1
# SPSS7E   0.70 -0.11 0.44 0.56   1
# SPSS8E   0.86  0.05 0.77 0.23   1
# SPSS9E   0.92 -0.02 0.83 0.17   1
# SPSS10E -0.06  0.64 0.39 0.61   1
```

Overall, none of the rotations were significantly better than the
default oblimin rotation.  
So, we decided to just stick with oblimin.
