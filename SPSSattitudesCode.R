## ----Loading Packages, echo = FALSE---------------------
library(readr)
library(haven)
library(tidyverse)
library(lavaan)
library(psych)
library(mirt)
library(rsample)
library(GPArotation)
library(MBESS)
library(REdaS)
library(faoutlier)

## ----Uploading data and cleaning------------------------
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

## ----Descriptive Stats----------------------------------
describe(spss.data)

# Contingency table of the counts
table(spss.data$SPSS1E)
table(spss.data$SPSS2E)
table(spss.data$SPSS3E)
table(spss.data$SPSS4E)
table(spss.data$SPSS5E)
table(spss.data$SPSS6E)
table(spss.data$SPSS7E)
table(spss.data$SPSS8E)
table(spss.data$SPSS9E)
table(spss.data$SPSS10E)

## Missing Data Calculations ##
table(is.na(spss.data))

# FALSE  TRUE 
#  1807     3

# 3 / (3 + 1807) = 0.001657459
# 0.001657459 * 100 = 0.1657459

## Less than 1% missing data, proceeding with complete case analyses

## -----------Assumptions----------
# Checking multivariate normality
mardia(spss.data)
# Kurtosis = 15.17 >4. Will not assume mvn.

## -------EFA Appropriateness------
# Barlett's Test of Sphericity which tests whether a matrix is significantly different from an identity matrix
bart_spher(spss.data, use = "complete.obs")
# p-value < 2.22e-16

# Kaiser-Meyer-Olkin Statistics
KMOS(spss.data, use = "complete.obs")
# KMO-Criterion: 0.8795382

## -----------Scatterplot matrix-----------------------
car::scatterplotMatrix(spss.data, smooth = F, regLine = F, col = 'black')

## -----------Listwise Deletion-----------
# Previous work suggests using listwise deletion when the missing data rates are extremely low (e.g., < 1%; Flora, 2018; Jakobsen et al., 2017).
spss.data <- spss.data[-c(33, 141, 104), ]
full.data <- full.data[-c(33, 141, 104), ]
# needed later for convergent/discriminant validity

spss.data <- data.frame(spss.data)
str(spss.data)

## ----Polychoric Correlations-----------------------------------
poly.spss.data <- psych::polychoric(spss.data)
write.csv(poly.spss.data$rho, file = "polyCorrTable.csv", row.names = TRUE)
# csv for manuscript writing

# Confidence Intervals for Polychoric Correlations
poly.spss.ci <- (cor.ci(spss.data, poly = TRUE, plot = FALSE))$ci
write.csv(poly.spss.ci, file = "polyCorrTableCI.csv", row.names = TRUE)
# csv for manuscript writing

# Polychoric correlations 
# SPSS1E SPSS2 SPSS3 SPSS4 SPSS5 SPSS6 SPSS7 SPSS8 SPSS9 SPSS10
# SPSS1E  1.00                                                         
# SPSS2E  0.38   1.00                                                  
# SPSS3E  0.35   0.69  1.00                                            
# SPSS4E  0.85   0.32  0.37  1.00                                      
# SPSS5E  0.70   0.35  0.27  0.73  1.00                                
# SPSS6E  0.55   0.27  0.23  0.56  0.58  1.00                          
# SPSS7E  0.56   0.23  0.14  0.56  0.55  0.46  1.00                    
# SPSS8E  0.78   0.32  0.35  0.74  0.73  0.61  0.60  1.00              
# SPSS9E  0.81   0.30  0.36  0.86  0.71  0.62  0.58  0.80  1.00        
# SPSS10E 0.26   0.50  0.49  0.23  0.12  0.05  0.02  0.24  0.12  1.00  
# 
# with tau of 
# 1     2      3    4
# SPSS1E  -1.7 -0.98 -0.099 0.96
# SPSS2E  -1.8 -0.87 -0.375 0.65
# SPSS3E  -1.6 -0.87 -0.198 0.90
# SPSS4E  -1.8 -0.87  0.014 1.19
# SPSS5E  -1.8 -1.19 -0.242 0.65
# SPSS6E  -1.3 -0.90 -0.099 0.63
# SPSS7E  -1.6 -0.60  0.085 0.85
# SPSS8E  -1.9 -1.13 -0.141 0.92
# SPSS9E  -1.8 -0.94  0.056 0.98
# SPSS10E -1.6 -0.76 -0.028 0.90

####
# Used polychoric correlations bc of 5-point Likert data
# All Qs are more correlated w each other than they are with 2,3,10. But 2,3, and 10 are more correlated to each other than other Qs.
####

## -------MAP and Parallel Analysis----------------------------------------
VSS(spss.data, fm = 'minres', cor = 'poly', plot = F)

# Very Simple Structure
# Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
#           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
# VSS complexity 1 achieves a maximimum of 0.87  with  1  factors
# VSS complexity 2 achieves a maximimum of 0.95  with  2  factors
# 
# The Velicer MAP achieves a minimum of 0.05  with  2  factors 
# BIC achieves a minimum of  NA  with  2  factors
# Sample Size adjusted BIC achieves a minimum of  NA  with  4  factors

fa.parallel(spss.data, fm = 'minres', cor = 'poly', fa ='both', n.iter=100)

# Parallel analysis suggests that the number of factors =  2  and the number of components =  2 

####
# Both MAP and PA suggest 2F 
# PA should be interpreted w caution for polychoric correlations
# Next, running 1F, 2F and 3F model (i.e. 1 above and 1 below suggested num. of factors) next to help determine which model is best
####

## -------1F EFA----------------------------------------
fa(r = spss.data, fm = 'minres', rotate = "oblimin", cor = 'poly', nfactors = 1)

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
# MR1
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
# MR1
# Correlation of (regression) scores with factors   0.97
# Multiple R square of scores with factors          0.95
# Minimum correlation of possible factor scores     0.90

####
# RMSR = 0.12 *BAD*
# Prop. var explained = 0.51 
# SPSS10E *BAD* factor loading (<.4) and communality (0.07)
# SPSS2E and SPSS3E factor loading almost <.4 and communality almost <.2
#
# Based on model fit (RMSR), 1F sucks
####

## ---------Outliers 1F ---------------
fS1 <- forward.search(spss.data, 1, criteria = c("mah", "GOF"))
gcdresult1 <- gCD(spss.data, 1)
ldresults1 <- LD(spss.data, 1)

plot(gcdresult1)
plot(fS1)
plot(ldresults1)

## ---------------2F EFA-----------------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 2)

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
# MR1  MR2
# SS loadings           4.74 1.75
# Proportion Var        0.47 0.17 PROP EXPLAINED PER FACTOR
# Cumulative Var        0.47 0.65 *
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
# MR1  MR2
# Correlation of (regression) scores with factors   0.98 0.91
# Multiple R square of scores with factors          0.95 0.84
# Minimum correlation of possible factor scores     0.90 0.67

####
# RMSR = 0.03 *WOW!* huge decrease by adding 1 more factor
# Prop. var explained = 0.65, 14% raw difference from 1F model
# No poor factor loadings or low communalities
# Column and row parsimony is pretty amazing
# Notice that all negatively worded items load onto factor 2 & all positively worded items load onto factor 1
# 2F prob wins, but let's try 3F next anyways
####

## ------- Outliers 2F ---------
fS2 <- forward.search(spss.data, 2, criteria = c("mah", "GOF"))
gcdresult2 <- gCD(spss.data, 2)
ldresults2 <- LD(spss.data, 2)

plot(gcdresult2)
plot(fS2)
plot(ldresults2)

## ----------------------------3F EFA---------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 3)

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

####
# RMSR = 0.02 *MEH* only decreased by 0.01 after adding an additional factor - not worth it bc RMSR always decreases when adding an additional factor.
# Prop. var explained = 0.69, 4% raw difference from 2F model
# No low communalities, BUT
  # In general, column and row parsimony is not nearly as good as 2F model
# Concluding that 2F wins bc improvements in model fit isn't worth it & column and row parsimony worse than 2F model 
####

## ----- Outliers 3F -------
fS3 <- forward.search(spss.data, 3, criteria = c("mah", "GOF"))
gcdresult3 <- gCD(spss.data, 3)
ldresults3 <- LD(spss.data, 3)

plot(gcdresult3)
plot(fS3)
plot(ldresults3)

## ---------------------2F EFA bentlerQ Rotation----------------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'bentlerQ', nfactors = 2)

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

####
# Very close to begin identifical to oblimin
####

## ---------------------2F EFA geominQ Rotation----------------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = 'geominQ', nfactors = 2)

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

####
# Almost identical to oblimin
####


## ------------------2F EFA quartimin Rotation-------------------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "quartimin", nfactors = 2)

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

####
# Identical to oblimin
####

## -----------------------2F EFA Promax Rotation--------------------------------
fa(r = spss.data, fm = 'minres', cor = 'poly', rotate = "Promax", nfactors = 2)

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

####
# Also, not very different from oblimin
####

# Decided to just stick with oblimin rotation as factor loadings did not really differ between the rotations

## -------------------------Bifactor EFA attempt------------------------------
psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = 'minres', rotate = 'oblimin', na.obs = NA, option = 'equal')

# Three factors are required for identification -- general factor loadings set to be equal. 
# Proceed with caution. 
# Think about redoing the analysis with alternative values of the 'option' setting.
# 
# Schmid-Leiman analysis 
# Call: psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = "minres", 
#                     rotate = "oblimin", option = "equal", na.obs = NA)
# 
# Schmid Leiman Factor loadings greater than  0.2 
# g   F1*   F2*   h2   u2   p2
# SPSS1E  0.61  0.64       0.78 0.22 0.47
# SPSS2E  0.54        0.60 0.66 0.34 0.45
# SPSS3E  0.55        0.62 0.69 0.31 0.44
# SPSS4E  0.60  0.67       0.80 0.20 0.45
# SPSS5E  0.52  0.62       0.66 0.34 0.41
# SPSS6E  0.42  0.52       0.45 0.55 0.39
# SPSS7E  0.38  0.54       0.44 0.56 0.33
# SPSS8E  0.59  0.65       0.77 0.23 0.45
# SPSS9E  0.58  0.70       0.83 0.17 0.41
# SPSS10E 0.37        0.50 0.39 0.61 0.35
# 
# With eigenvalues of:
#   g F1* F2* 
#   2.7 2.7 1.0 
# 
# general/max  1   max/min =   2.7
# mean percent general =  0.42    with sd =  0.05 and cv of  0.11 
# 
# The orthogonal loadings were 
# Unstandardized loadings based upon covariance matrix
# F1   F2   h2   u2   H2   U2
# SPSS1E  0.85 0.26 0.78 0.22 0.78 0.22
# SPSS2E  0.22 0.78 0.66 0.34 0.66 0.34
# SPSS3E  0.20 0.81 0.69 0.31 0.69 0.31
# SPSS4E  0.87 0.23 0.80 0.20 0.80 0.20
# SPSS5E  0.80 0.16 0.66 0.34 0.66 0.34
# SPSS6E  0.66 0.10 0.45 0.55 0.45 0.55
# SPSS7E  0.66 0.03 0.44 0.56 0.44 0.56
# SPSS8E  0.85 0.22 0.77 0.23 0.77 0.23
# SPSS9E  0.90 0.16 0.83 0.17 0.83 0.17
# SPSS10E 0.06 0.62 0.39 0.61 0.39 0.61
# 
# F1   F2
# SS loadings    4.61 1.88
# Proportion Var 0.46 0.19
# Cumulative Var 0.46 0.65
# 
# The degrees of freedom are 26  and the fit is  0.51 
# 
# The root mean square of the residuals is  0.03 
# The df corrected root mean square of the residuals is  0.04

## --------------Reliability per Factor-----------------------------------------
# split dataset into each factor
spss.data.f1 <- spss.data %>% select(-c(SPSS2E, SPSS3E, SPSS10E)) 
spss.data.f2 <- spss.data %>% select(c(SPSS2E, SPSS3E, SPSS10E)) 

# psych::omega()
## poly = TRUE because we want to us the polychoric correlation matrix instead of Pearson because categorical data
## since nfactors = 1, only omega total is meaningful 
omega(m = spss.data.f1, poly = TRUE, plot = F, nfactors = 1) 
# Omega Total 0.93 
omega(m = spss.data.f2, poly = TRUE, plot = F, nfactors = 1) 
# Omega Total 0.8 
  # warning message regarding 'non-finite result is doubtful' refers to the NA or NaN values in the output. They should not be trusted, but exist because the input provided has NA values

# MBESS:ci.reliability() for 95% CI

# The code below follows Flora (2020), but it runs infinitely...
# ci.reliability(spss.data.f1, type="categorical", interval.type="perc")
# ci.reliability(spss.data.f2, type="categorical", interval.type="perc")

# Changed the interval.type to = "bca" because the ci.reliability() documentation recommends it for categorical omega, but it also runs infinitely...
# ci.reliability(spss.data.f1, type="categorical", interval.type="bca")
# ci.reliability(spss.data.f2, type="categorical", interval.type="bca")

# The code below runs, but does not account for the categorical nature of the items - therefore possibly inappropriate estimate of the scale's reliability
ci.reliability(spss.data.f1) 
# est 0.9077046, ci.lower 0.8816747, ci.upper 0.9337345
ci.reliability(spss.data.f2) 
# est 0.7429931, ci.lower 0.6599966, ci.upper 0.8259896

# Overall, I think MBESS::ci.reliability will not be appropriate here and psych::omega() is preferred

## --------------Reliability Overall-----------------------------------------
# note that this is not appropriate for our 2F model, but may be requested by reviewers

# psych::omega()
omega(m = spss.data, poly = TRUE, plot = F, nfactors = 2) 
# Omega Total for total scores = 0.93, for F1 = 0.94 and for F2 = 0.80 ; side note: I probably could have ran this instead of splitting the data into each of its factors?

# MBESS:ci.reliability() for 95% CI
# ci.reliability(spss.data, type="categorical", interval.type="perc") # again, runs infinitely...
# ci.reliability(spss.data, type="categorical", interval.type="bca") # also runs infinitely...
ci.reliability(spss.data) 
# runs, but not appropriate because does not account for categorical nature of items. est = 0.8677201, ci.lower = 0.8353075, ci.upper = 0.9001326

## --------Convergent Validity-------
# Quantitative Attitudes
qa.data <- full.data %>% select(
  MA1E:MA8E
) %>% select(
  -MA6E 
  # removed 'Statistics is a not a worthwhile or necessary subject' based on previous validation paper 
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
# SPSS Attitudes 
sa.data <- spss.data %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qa.data$total, sa.data$total) 
# r = 0.2533858  ; p = 0.0006434 ; 95% CI [0.1104162 0.3860816]
car::scatterplot(qa.data$total, sa.data$total)

## --------Discriminant Validity------- 
# Quantitative Anxiety
qanx.data <- full.data %>% select(
  QANX1E:QANX4E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qanx.data$total, sa.data$total) 
# r = -0.0787332  ; p = 0.2962 ; 95% CI [ -0.2232328  0.0691523]
car::scatterplot(qanx.data$total, sa.data$total)

# Quantitative Hindrances
qh.data <- full.data %>% select(
  QHIND1E:QHIND5E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qh.data$total, sa.data$total) 
# r = -0.05655468  ; p = 0.4534 ; 95% CI [-0.20195941  0.09128939]
car::scatterplot(qh.data$total, sa.data$total)

## --------Exploratory Convergent / Discriminant Validity?----
# The above were predictions we made a priori - below is me playing around

# Quantitative Influences
qi.data <- full.data %>% select(
  QINFL1E:QINFL7E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qi.data$total, sa.data$total) 
# r = 0.07819996   ; p = 0.2995 ; 95% CI [-0.06968627  0.22272289]
car::scatterplot(qi.data$total, sa.data$total)

# Quantitative Success Factors
qsf.data <- full.data %>% select(
  QSF1E:QSF4E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qsf.data$total, sa.data$total) 
# r = 0.05973762  ; p = 0.4283 ; 95% CI [ -0.08812135  0.20502090]
car::scatterplot(qsf.data$total, sa.data$total)

# Quantitative Self-Confidence
qsc.data <- full.data %>% select(
  QSC1E:QSC4E
) %>% mutate(
  QSC2E = car::recode(QSC2E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1"), 
  # reverse code 'I feel insecure in my math/statistics abilities'
  QSC3E = car::recode(QSC3E, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1"), 
  # reverse code 'I find it hard to think in terms of symbols'
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qsc.data$total, sa.data$total) 
# r = 0.1372719     ; p = 0.06767 ; 95% CI [ -0.01001498  0.27872892]
car::scatterplot(qsc.data$total, sa.data$total)

# Quantitative Self-Efficacy
qse.data <- full.data %>% select(
  QSE1E:QSE6E
) %>% select(
  -c(QSE1E, QSE2E)
  # removed “Balance my checkbook without a mistake” based on previous validation study 
  # removed “Read a table, such as what is in back of math books, to determine an answer.” based on previous validation study 
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Correlation
cor.test(qse.data$total, sa.data$total) 
# r = 0.2474466  ; p = 0.0008684 ; 95% CI [0.1041526 0.3806764]
car::scatterplot(qse.data$total, sa.data$total)

end <- "end"
