################################
## Step One: Loading Packages ##
################################

library(readr)
library(haven)
library(tidyverse)
library(lavaan)
library(psych)
library(mirt)
library(rsample)

############################
## Step Two: Loading Data ##
############################

Combined_URI_Mturk_Dissertation_Data_12_26_16 <- read_csv("C:/Users/zkunicki/Desktop/FullData.csv")

#########################################
## Step Three: Separate By Data Source ##
#########################################

MTurkData <- subset(Combined_URI_Mturk_Dissertation_Data_12_26_16, URI1MTurk2 == "2")
URIData <- subset(Combined_URI_Mturk_Dissertation_Data_12_26_16, URI1MTurk2 == "1")

# Selecting just variables of interest

MTurk.Resilience.Data <- MTurkData %>% select(RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8, RS9, RS10,
                                              RS11, RS12, RS13, RS14, RS15, RS16, RS17, RS18, RS19,
                                              RS20, RS21, RS22, RS23, RS24, RS25)

URI.Resilience.Data <- URIData %>% select(RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8, RS9, RS10,
                                              RS11, RS12, RS13, RS14, RS15, RS16, RS17, RS18, RS19,
                                              RS20, RS21, RS22, RS23, RS24, RS25)

#######################################
## Step Four: Descriptive Statistics ##
#######################################
describe(MTurk.Resilience.Data)

table(MTurk.Resilience.Data$RS1)
table(MTurk.Resilience.Data$RS2)
table(MTurk.Resilience.Data$RS3)
table(MTurk.Resilience.Data$RS4)
table(MTurk.Resilience.Data$RS5)
table(MTurk.Resilience.Data$RS6)
table(MTurk.Resilience.Data$RS7)
table(MTurk.Resilience.Data$RS8)
table(MTurk.Resilience.Data$RS9)
table(MTurk.Resilience.Data$RS10)
table(MTurk.Resilience.Data$RS11)
table(MTurk.Resilience.Data$RS12)
table(MTurk.Resilience.Data$RS13)
table(MTurk.Resilience.Data$RS14)
table(MTurk.Resilience.Data$RS15)
table(MTurk.Resilience.Data$RS16)
table(MTurk.Resilience.Data$RS17)
table(MTurk.Resilience.Data$RS18)
table(MTurk.Resilience.Data$RS19)
table(MTurk.Resilience.Data$RS20)
table(MTurk.Resilience.Data$RS21)
table(MTurk.Resilience.Data$RS22)
table(MTurk.Resilience.Data$RS23)
table(MTurk.Resilience.Data$RS24)
table(MTurk.Resilience.Data$RS25)

describe(URI.Resilience.Data)

table(URI.Resilience.Data$RS1)
table(URI.Resilience.Data$RS2)
table(URI.Resilience.Data$RS3)
table(URI.Resilience.Data$RS4)
table(URI.Resilience.Data$RS5)
table(URI.Resilience.Data$RS6)
table(URI.Resilience.Data$RS7)
table(URI.Resilience.Data$RS8)
table(URI.Resilience.Data$RS9)
table(URI.Resilience.Data$RS10)
table(URI.Resilience.Data$RS11)
table(URI.Resilience.Data$RS12)
table(URI.Resilience.Data$RS13)
table(URI.Resilience.Data$RS14)
table(URI.Resilience.Data$RS15)
table(URI.Resilience.Data$RS16)
table(URI.Resilience.Data$RS17)
table(URI.Resilience.Data$RS18)
table(URI.Resilience.Data$RS19)
table(URI.Resilience.Data$RS20)
table(URI.Resilience.Data$RS21)
table(URI.Resilience.Data$RS22)
table(URI.Resilience.Data$RS23)
table(URI.Resilience.Data$RS24)
table(URI.Resilience.Data$RS25)

## Missing Data Calculations ##

table(is.na(MTurk.Resilience.Data))

# FALSE  TRUE 
# 12475    25 
# 25 / (25 + 12475) = 0.002

table(is.na(URI.Resilience.Data))

# FALSE  TRUE 
# 17957    43 
# 43 / (43 + 17957) = 0.002

## Less than 1% missing data, proceeding with complete case analyses.

############################################
## Step Five: Splitting Into EFA/CFA Sets ##
############################################
#Normally, the testing set is the smaller one, but in this case we prefer more data for the CFA. So, we're flipping it.
#This is using a 75/25 split, or the default of the package.

EFA_CFA_MTurk_Split <- initial_split(MTurk.Resilience.Data)

EFA.MTurk <- testing(EFA_CFA_MTurk_Split)
CFA.MTurk <- training(EFA_CFA_MTurk_Split)

EFA_CFA_URI_Split <- initial_split(URI.Resilience.Data)

EFA.URI <- testing(EFA_CFA_URI_Split)
CFA.URI <- training(EFA_CFA_URI_Split)

############################
## Step Six: MAP/Parallel ##
############################

VSS(EFA.MTurk, plot = F)

# The Velicer MAP achieves a minimum of 0.02  with  2  factors 

fa.parallel(EFA.MTurk, fa = "both")

# Parallel analysis suggests that the number of factors =  3  and the number of components =  2 

VSS(EFA.URI, plot = F)

# The Velicer MAP achieves a minimum of 0.02  with  2  factors 

fa.parallel(EFA.URI, fa = "both")

# Parallel analysis suggests that the number of factors =  5  and the number of components =  2 

# Result show 2 factors/components with MAP and parallel, so assuming the two factor structure.

######################
## Step Seven: EFAs ##
######################

#EFA

fa(EFA.MTurk, fm = "ml", nfactors = 2, rotate = "Promax")

# I always prefer promax rotation and maximum likelihood extraction. These are not universally agreed upon.
# ML extraction is highly prone to Heywood cases. Promax rotation allows for correlations between factors.

# Factor Analysis using method =  ml
# Call: fa(r = EFA.MTurk, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2    h2   u2 com
# RS1   0.36  0.33 0.386 0.61 2.0    DROP, BELOW |.40|
# RS2   0.70  0.11 0.594 0.41 1.1    
# RS3   0.78 -0.24 0.435 0.57 1.2
# RS4   0.70  0.06 0.537 0.46 1.0
# RS5   0.79 -0.18 0.479 0.52 1.1
# RS6   0.53  0.37 0.654 0.35 1.8    
# RS7   0.21  0.50 0.419 0.58 1.3
# RS8  -0.03  0.79 0.594 0.41 1.0
# RS9   0.36  0.43 0.492 0.51 1.9    
# RS10  0.57  0.32 0.653 0.35 1.6    
# RS11 -0.05  0.16 0.019 0.98 1.2    DROP, BELOW |.40|
# RS12 -0.11  0.31 0.070 0.93 1.2   
# RS13  0.68  0.14 0.594 0.41 1.1
# RS14  0.13  0.56 0.424 0.58 1.1
# RS15  0.43  0.39 0.543 0.46 2.0    
# RS16  0.73 -0.08 0.465 0.54 1.0
# RS17  0.31  0.56 0.627 0.37 1.6    
# RS18  0.63  0.19 0.581 0.42 1.2
# RS19  0.68  0.14 0.594 0.41 1.1
# RS20  0.58 -0.08 0.291 0.71 1.0
# RS21  0.16  0.53 0.415 0.58 1.2
# RS22 -0.23  0.66 0.309 0.69 1.2
# RS23  0.57  0.36 0.708 0.29 1.7    
# RS24  0.09  0.60 0.435 0.56 1.0
# RS25  0.18  0.31 0.201 0.80 1.6    DROP, BELOW |.40|
# 
# ML1  ML2
# SS loadings           6.87 4.65
# Proportion Var        0.27 0.19
# Cumulative Var        0.27 0.46
# Proportion Explained  0.60 0.40
# Cumulative Proportion 0.60 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.61
# ML2 0.61 1.00
# 
# Mean item complexity =  1.3
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  300  and the objective function was  15.95 with Chi Square of  1832.14
# The degrees of freedom for the model are 251  and the objective function was  3.78 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  124 with the empirical chi square  280.97  with prob <  0.094 
# The total number of observations was  125  with Likelihood Chi Square =  428.89  with prob <  1.8e-11 
# 
# Tucker Lewis Index of factoring reliability =  0.859
# RMSEA index =  0.084  and the 90 % confidence intervals are  0.063 0.088
# BIC =  -783.02
# Fit based upon off diagonal values = 0.98
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.96 0.94
# Multiple R square of scores with factors          0.93 0.89
# Minimum correlation of possible factor scores     0.86 0.77

#Dropping items, re-running EFA
EFA.MTurk.2 <- EFA.MTurk %>% select(-RS1, -RS11, -RS25)

fa(EFA.MTurk.2, fm = "ml", nfactors = 2, rotate = "Promax")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.MTurk.2, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2    h2   u2 com
# RS2   0.66  0.16 0.587 0.41 1.1
# RS3   0.77 -0.21 0.439 0.56 1.2
# RS4   0.67  0.10 0.538 0.46 1.0
# RS5   0.77 -0.15 0.486 0.51 1.1
# RS6   0.49  0.42 0.661 0.34 2.0  DROP, COMPLEX
# RS7   0.19  0.51 0.421 0.58 1.3
# RS8  -0.06  0.81 0.601 0.40 1.0
# RS9   0.33  0.46 0.492 0.51 1.8
# RS10  0.53  0.38 0.655 0.34 1.8
# RS12 -0.09  0.29 0.062 0.94 1.2  DROP, BELOW |.40|
# RS13  0.65  0.19 0.604 0.40 1.2
# RS14  0.11  0.57 0.416 0.58 1.1
# RS15  0.40  0.43 0.544 0.46 2.0  DROP, COMPLEX
# RS16  0.72 -0.06 0.470 0.53 1.0
# RS17  0.29  0.58 0.622 0.38 1.5
# RS18  0.60  0.24 0.579 0.42 1.3
# RS19  0.66  0.17 0.590 0.41 1.1
# RS20  0.58 -0.06 0.294 0.71 1.0
# RS21  0.13  0.57 0.424 0.58 1.1
# RS22 -0.22  0.64 0.288 0.71 1.2
# RS23  0.54  0.40 0.707 0.29 1.8  DROP, COMPLEX
# RS24  0.07  0.60 0.422 0.58 1.0
# 
# ML1  ML2
# SS loadings           6.23 4.67
# Proportion Var        0.28 0.21
# Cumulative Var        0.28 0.50
# Proportion Explained  0.57 0.43
# Cumulative Proportion 0.57 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.59
# ML2 0.59 1.00
# 
# Mean item complexity =  1.3
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  231  and the objective function was  14.32 with Chi Square of  1658.69
# The degrees of freedom for the model are 188  and the objective function was  2.83 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.06 
# 
# The harmonic number of observations is  124 with the empirical chi square  184.11  with prob <  0.57 
# The total number of observations was  125  with Likelihood Chi Square =  324.14  with prob <  2.6e-09 
# 
# Tucker Lewis Index of factoring reliability =  0.881
# RMSEA index =  0.084  and the 90 % confidence intervals are  0.062 0.09
# BIC =  -583.58
# Fit based upon off diagonal values = 0.98
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.96 0.94
# Multiple R square of scores with factors          0.92 0.89
# Minimum correlation of possible factor scores     0.84 0.78

#Dropping items, re-running EFA
EFA.MTurk.3 <- EFA.MTurk.2 %>% select(-RS6, -RS12, -RS15, -RS23)

fa(EFA.MTurk.3, fm = "ml", nfactors = 2, rotate = "Promax")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.MTurk.3, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2   h2   u2 com
# RS2   0.64  0.18 0.58 0.42 1.2
# RS3   0.76 -0.17 0.45 0.55 1.1
# RS4   0.66  0.10 0.52 0.48 1.0
# RS5   0.77 -0.14 0.48 0.52 1.1
# RS7   0.17  0.53 0.42 0.58 1.2
# RS8  -0.10  0.84 0.61 0.39 1.0
# RS9   0.30  0.47 0.48 0.52 1.7
# RS10  0.49  0.39 0.62 0.38 1.9
# RS13  0.62  0.21 0.59 0.41 1.2
# RS14  0.08  0.58 0.41 0.59 1.0
# RS16  0.72 -0.06 0.47 0.53 1.0
# RS17  0.26  0.61 0.64 0.36 1.4
# RS18  0.56  0.28 0.59 0.41 1.5
# RS19  0.65  0.18 0.60 0.40 1.2
# RS20  0.61 -0.06 0.32 0.68 1.0
# RS21  0.09  0.59 0.41 0.59 1.0
# RS22 -0.24  0.64 0.28 0.72 1.3
# RS24  0.04  0.63 0.43 0.57 1.0
# 
# ML1  ML2
# SS loadings           4.99 3.94
# Proportion Var        0.28 0.22
# Cumulative Var        0.28 0.50
# Proportion Explained  0.56 0.44
# Cumulative Proportion 0.56 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.61
# ML2 0.61 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  153  and the objective function was  10.14 with Chi Square of  1187.57
# The degrees of freedom for the model are 118  and the objective function was  1.55 
# 
# The root mean square of the residuals (RMSR) is  0.05 
# The df corrected root mean square of the residuals is  0.06 
# 
# The harmonic number of observations is  125 with the empirical chi square  102.13  with prob <  0.85 
# The total number of observations was  125  with Likelihood Chi Square =  179.92  with prob <  0.00021 
# 
# Tucker Lewis Index of factoring reliability =  0.921
# RMSEA index =  0.071  and the 90 % confidence intervals are  0.045 0.084
# BIC =  -389.82
# Fit based upon off diagonal values = 0.99
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.94
# Multiple R square of scores with factors          0.91 0.88
# Minimum correlation of possible factor scores     0.82 0.77

#Retain all items

fa(EFA.URI, nfactors = 2, rotate = "Promax", fm = "ml")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.URI, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#       ML1   ML2    h2   u2 com
# RS1   0.67 -0.21 0.305 0.69 1.2
# RS2   0.75 -0.16 0.434 0.57 1.1
# RS3   0.43 -0.04 0.159 0.84 1.0
# RS4   0.68  0.07 0.531 0.47 1.0
# RS5   0.21  0.12 0.094 0.91 1.6 DROP, BELOW |.40|
# RS6   0.66  0.17 0.609 0.39 1.1
# RS7   0.65  0.04 0.461 0.54 1.0
# RS8   0.29  0.43 0.429 0.57 1.8
# RS9   0.47  0.25 0.445 0.56 1.5
# RS10  0.79  0.03 0.658 0.34 1.0 DROP, BELOW |.40|
# RS11 -0.22  0.17 0.027 0.97 1.9
# RS12 -0.22  0.47 0.132 0.87 1.4
# RS13  0.22  0.32 0.249 0.75 1.8 DROP, BELOW |.40|
# RS14  0.52  0.18 0.418 0.58 1.2
# RS15  0.53  0.29 0.567 0.43 1.6
# RS16  0.29  0.49 0.511 0.49 1.6
# RS17 -0.06  0.89 0.726 0.27 1.0
# RS18  0.08  0.54 0.356 0.64 1.0
# RS19  0.05  0.58 0.379 0.62 1.0
# RS20  0.29  0.16 0.170 0.83 1.6 DROP, BELOW |.40|
# RS21  0.16  0.53 0.427 0.57 1.2
# RS22 -0.17  0.53 0.193 0.81 1.2
# RS23 -0.06  0.53 0.245 0.75 1.0
# RS24  0.12  0.38 0.222 0.78 1.2 DROP, BELOW |.40|
# RS25  0.15  0.29 0.165 0.84 1.5 DROP, BELOW |.40|
# 
# ML1  ML2
# SS loadings           4.88 4.04
# Proportion Var        0.20 0.16
# Cumulative Var        0.20 0.36
# Proportion Explained  0.55 0.45
# Cumulative Proportion 0.55 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.66
# ML2 0.66 1.00
# 
# Mean item complexity =  1.3
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  300  and the objective function was  11.13 with Chi Square of  1889.48
# The degrees of freedom for the model are 251  and the objective function was  2.94 
# 
# The root mean square of the residuals (RMSR) is  0.07 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  179 with the empirical chi square  461.17  with prob <  1.4e-14 
# The total number of observations was  180  with Likelihood Chi Square =  495.57  with prob <  3.3e-18 
# 
# Tucker Lewis Index of factoring reliability =  0.814
# RMSEA index =  0.078  and the 90 % confidence intervals are  0.064 0.083
# BIC =  -807.87
# Fit based upon off diagonal values = 0.96
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.94
# Multiple R square of scores with factors          0.91 0.89
# Minimum correlation of possible factor scores     0.81 0.78

# Already, we are seeing that more items will be dropped in the URI sample. This is interesting.

EFA.URI.2 <- EFA.URI %>% select(-RS5, -RS10, -RS13, -RS20, -RS24, -RS25)

fa(EFA.URI.2, nfactors = 2, rotate = "Promax", fm = "ml")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.URI.2, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2    h2   u2 com
# RS1  -0.25  0.75 0.374 0.63 1.2 
# RS2  -0.19  0.83 0.512 0.49 1.1
# RS3   0.00  0.39 0.152 0.85 1.0 DROP, BELOW |.40|
# RS4   0.10  0.69 0.582 0.42 1.0
# RS6   0.27  0.56 0.590 0.41 1.4
# RS7   0.13  0.59 0.464 0.54 1.1
# RS8   0.46  0.25 0.432 0.57 1.5
# RS9   0.35  0.36 0.418 0.58 2.0 DROP, BELOW |.40|
# RS11  0.16 -0.22 0.026 0.97 1.8 DROP, BELOW |.40|
# RS12  0.41 -0.18 0.102 0.90 1.4
# RS14  0.27  0.40 0.372 0.63 1.8
# RS15  0.38  0.42 0.532 0.47 2.0
# RS16  0.57  0.21 0.525 0.48 1.3
# RS17  0.99 -0.19 0.763 0.24 1.1
# RS18  0.59  0.01 0.363 0.64 1.0
# RS19  0.59  0.04 0.379 0.62 1.0
# RS21  0.61  0.06 0.427 0.57 1.0
# RS22  0.47 -0.12 0.158 0.84 1.1
# RS23  0.44  0.02 0.200 0.80 1.0
# 
# ML1  ML2
# SS loadings           3.92 3.45
# Proportion Var        0.21 0.18
# Cumulative Var        0.21 0.39
# Proportion Explained  0.53 0.47
# Cumulative Proportion 0.53 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.67
# ML2 0.67 1.00
# 
# Mean item complexity =  1.3
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  171  and the objective function was  7.94 with Chi Square of  1364.29
# The degrees of freedom for the model are 134  and the objective function was  1.51 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  179 with the empirical chi square  211.08  with prob <  2.4e-05 
# The total number of observations was  180  with Likelihood Chi Square =  257.99  with prob <  7e-10 
# 
# Tucker Lewis Index of factoring reliability =  0.866
# RMSEA index =  0.076  and the 90 % confidence intervals are  0.059 0.085
# BIC =  -437.87
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.94
# Multiple R square of scores with factors          0.90 0.88
# Minimum correlation of possible factor scores     0.81 0.75

EFA.URI.3 <- EFA.URI.2 %>% select(-RS3, -RS9, -RS11)

fa(EFA.URI.3, nfactors = 2, rotate = "Promax", fm = "ml")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.URI.3, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2    h2   u2 com
# RS1  -0.28  0.77 0.379 0.62 1.3
# RS2  -0.22  0.85 0.513 0.49 1.1
# RS4   0.06  0.73 0.595 0.40 1.0
# RS6   0.25  0.58 0.588 0.41 1.4
# RS7   0.10  0.60 0.456 0.54 1.1
# RS8   0.46  0.25 0.425 0.57 1.5
# RS12  0.39 -0.14 0.098 0.90 1.3 DROP, BELOW |.40|
# RS14  0.25  0.42 0.372 0.63 1.6
# RS15  0.35  0.44 0.527 0.47 1.9
# RS16  0.54  0.25 0.536 0.46 1.4
# RS17  0.96 -0.15 0.754 0.25 1.0
# RS18  0.59  0.03 0.365 0.64 1.0
# RS19  0.58  0.06 0.386 0.61 1.0
# RS21  0.60  0.08 0.427 0.57 1.0
# RS22  0.45 -0.09 0.157 0.84 1.1
# RS23  0.41  0.04 0.195 0.81 1.0
# 
# ML1  ML2
# SS loadings           3.53 3.24
# Proportion Var        0.22 0.20
# Cumulative Var        0.22 0.42
# Proportion Explained  0.52 0.48
# Cumulative Proportion 0.52 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.68
# ML2 0.68 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  120  and the objective function was  6.92 with Chi Square of  1195.57
# The degrees of freedom for the model are 89  and the objective function was  1.14 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  179 with the empirical chi square  152.56  with prob <  3.2e-05 
# The total number of observations was  180  with Likelihood Chi Square =  194.97  with prob <  6.8e-10 
# 
# Tucker Lewis Index of factoring reliability =  0.866
# RMSEA index =  0.085  and the 90 % confidence intervals are  0.066 0.097
# BIC =  -267.2
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.94
# Multiple R square of scores with factors          0.90 0.88
# Minimum correlation of possible factor scores     0.79 0.75

EFA.URI.4 <- EFA.URI.3 %>% select(-RS12)

fa(EFA.URI.4, nfactors = 2, rotate = "Promax", fm = "ml")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.URI.4, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
#        ML1   ML2   h2   u2 com
# RS1  -0.28  0.78 0.38 0.62 1.3
# RS2  -0.21  0.85 0.52 0.48 1.1
# RS4   0.08  0.71 0.60 0.40 1.0
# RS6   0.27  0.55 0.59 0.41 1.5
# RS7   0.12  0.59 0.45 0.55 1.1
# RS8   0.47  0.23 0.42 0.58 1.5
# RS14  0.27  0.39 0.37 0.63 1.8 DROP, BELOW |.40|
# RS15  0.38  0.41 0.53 0.47 2.0
# RS16  0.56  0.22 0.54 0.46 1.3
# RS17  1.00 -0.20 0.75 0.25 1.1
# RS18  0.62 -0.01 0.37 0.63 1.0
# RS19  0.61  0.02 0.39 0.61 1.0
# RS21  0.63  0.03 0.43 0.57 1.0
# RS22  0.45 -0.10 0.15 0.85 1.1
# RS23  0.42  0.03 0.19 0.81 1.0
# 
# ML1  ML2
# SS loadings           3.60 3.07
# Proportion Var        0.24 0.20
# Cumulative Var        0.24 0.45
# Proportion Explained  0.54 0.46
# Cumulative Proportion 0.54 1.00
# 
# With factor correlations of 
# ML1 ML2
# ML1 1.0 0.7
# ML2 0.7 1.0
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  105  and the objective function was  6.77 with Chi Square of  1173.06
# The degrees of freedom for the model are 76  and the objective function was  1.08 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  179 with the empirical chi square  140.93  with prob <  8.8e-06 
# The total number of observations was  180  with Likelihood Chi Square =  184.99  with prob <  4.5e-11 
# 
# Tucker Lewis Index of factoring reliability =  0.858
# RMSEA index =  0.093  and the 90 % confidence intervals are  0.073 0.106
# BIC =  -209.68
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.93
# Multiple R square of scores with factors          0.90 0.87
# Minimum correlation of possible factor scores     0.80 0.75

EFA.URI.5 <- EFA.URI.4 %>% select(-RS14)

fa(EFA.URI.5, nfactors = 2, rotate = "Promax", fm = "ml")

# Factor Analysis using method =  ml
# Call: fa(r = EFA.URI.5, nfactors = 2, rotate = "Promax", fm = "ml")
# Standardized loadings (pattern matrix) based upon correlation matrix
# ML1   ML2   h2   u2 com
# RS1  -0.26  0.77 0.39 0.61 1.2
# RS2  -0.19  0.83 0.52 0.48 1.1
# RS4   0.09  0.72 0.61 0.39 1.0
# RS6   0.30  0.52 0.57 0.43 1.6
# RS7   0.14  0.57 0.46 0.54 1.1
# RS8   0.48  0.23 0.42 0.58 1.4
# RS15  0.40  0.37 0.51 0.49 2.0
# RS16  0.57  0.21 0.54 0.46 1.3
# RS17  0.99 -0.20 0.75 0.25 1.1
# RS18  0.61 -0.01 0.37 0.63 1.0
# RS19  0.60  0.04 0.40 0.60 1.0
# RS21  0.64  0.02 0.43 0.57 1.0
# RS22  0.44 -0.09 0.15 0.85 1.1
# RS23  0.42  0.03 0.19 0.81 1.0
# 
# ML1  ML2
# SS loadings           3.53 2.79
# Proportion Var        0.25 0.20
# Cumulative Var        0.25 0.45
# Proportion Explained  0.56 0.44
# Cumulative Proportion 0.56 1.00
# 
# With factor correlations of 
# ML1  ML2
# ML1 1.00 0.68
# ML2 0.68 1.00
# 
# Mean item complexity =  1.2
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  91  and the objective function was  6.21 with Chi Square of  1078.2
# The degrees of freedom for the model are 64  and the objective function was  0.93 
# 
# The root mean square of the residuals (RMSR) is  0.06 
# The df corrected root mean square of the residuals is  0.07 
# 
# The harmonic number of observations is  179 with the empirical chi square  126.05  with prob <  6e-06 
# The total number of observations was  180  with Likelihood Chi Square =  160.09  with prob <  3.4e-10 
# 
# Tucker Lewis Index of factoring reliability =  0.86
# RMSEA index =  0.095  and the 90 % confidence intervals are  0.074 0.109
# BIC =  -172.26
# Fit based upon off diagonal values = 0.97
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   0.95 0.93
# Multiple R square of scores with factors          0.90 0.86
# Minimum correlation of possible factor scores     0.80 0.73

#Retain all items

#EFA results suggest a 14 item measure if normed on UGs, but an 18 measure if normed on MTurk.
#Let's try to re-create both factor structures in the opposite samples and see what happens.

######################
## Step Eight: CFAs ##
######################

#Building lavaan models

MTurk.Normed.Model <- '
f1 =~ RS2 + RS3 + RS4 + RS5 + RS10 + RS13 + RS16 + RS18 + RS19 + RS20 
f2 =~ RS7 + RS8 + RS9 + RS14 + RS17 + RS21 + RS22 + RS24
f1 ~~ f2 '

URI.Normed.Model <- '
f1 =~ RS8 + RS15 + RS16 + RS17 + RS18 + RS19 + RS21 + RS22 + RS23
f2 =~ RS1 + RS2 + RS4 + RS6 + RS7
f1 ~~ f2'

MTurk_on_MTurk <-cfa(MTurk.Normed.Model, data = CFA.MTurk)
summary(MTurk_on_MTurk, fit.measure = T)

#x2 = 503.44, df = 134, p < .001
#CFI = 0.90
#RMSEA = 0.09, 90% CI 0.08, 0.10
#SRMR = 0.06

MTurk_on_URI <- cfa(URI.Normed.Model, data = CFA.MTurk)
summary(MTurk_on_URI, fit.measure = T)

#x2 = 327.42, df = 76, p < .001
#CFI = 0.92
#RMSEA = 0.10, 90% CI 0.08, 0.11
#SRMR = 0.05

URI_on_MTurk <- cfa(MTurk.Normed.Model, data = CFA.URI)
summary(URI_on_MTurk, fit.measure = T)

#x2 = 598.85, df = 134, p < .001
#CFI = 0.87
#RMSEA = 0.08, 90% CI 0.08, 0.09
#SRMR = 0.06

URI_on_URI <- cfa(URI.Normed.Model, data = CFA.URI)
summary(URI_on_URI, fit.measure = T)

#x2 = 432.80, df = 76, p < .001
#CFI = 0.88
#RMSEA = 0.10, 90% CI 0.09, 0.10
#SRMR = 0.06

#The MTurk sample shows good fit to the data in both models. The URI sample shows poor fit in both models.
#Something fishy is happening here. How do we decide which model to select if both show poor fit?
#Seems reasonable to go with the MTurk model. The MTurk model is more balanced on both factors, and MTurk data tend
#to be more reflective of the "general population" compared to college students based on literature and expert review.

####################
## Step Nine: IRT ##
####################

MTurk_IRT <- cfa(MTurk.Normed.Model, data = CFA.MTurk, estimator = "DWLS", 
                 ordered = c("RS2", "RS3", "RS4", "RS5", "RS10", "RS13", "RS16", "RS18", "RS19", "RS20",
                             "RS7", "RS8", "RS9", "RS14", "RS17", "RS21", "RS22", "RS24"),
                 std.lv=T, parameterization='theta')

summary(MTurk_IRT, fit.measure = T)

#x2 = 331.07, df = 134, p < .001
#CFI = 1.00
#RMSEA = 0.06, 90% CI 0.06, 0.07
#SRMR = 0.05

URI_IRT <- cfa(MTurk.Normed.Model, data = CFA.URI, estimator = "DWLS", 
                  ordered = c("RS2", "RS3", "RS4", "RS5", "RS10", "RS13", "RS16", "RS18", "RS19", "RS20",
                              "RS7", "RS8", "RS9", "RS14", "RS17", "RS21", "RS22", "RS24"),
                  std.lv=T, parameterization='theta')

summary(URI_IRT, fit.measure = T)

#x2 = 541.86, df = 134, p < .001
#CFI = 0.99
#RMSEA = 0.08, 90% CI 0.07, 0.08
#SRMR = 0.06

#Good fit in both samples with IRT, awesome!

###########################
## Step Ten: Reliability ##
###########################

#Selecting for full measure, factor 1, factor 2

MTurk.Full <- MTurkData %>% select(RS2, RS3, RS4, RS5, RS10, RS13, RS16, RS18, RS19, RS20,
                                    RS7, RS8, RS9, RS14, RS17, RS21, RS22, RS24)

MTurk.F1 <- MTurkData %>% select(RS2, RS3, RS4, RS5, RS10, RS13, RS16, RS18, RS19, RS20)

MTurk.F2 <- MTurkData %>% select(RS7, RS8, RS9, RS14, RS17, RS21, RS22, RS24)

URI.Full <- URIData %>% select(RS2, RS3, RS4, RS5, RS10, RS13, RS16, RS18, RS19, RS20,
                                   RS7, RS8, RS9, RS14, RS17, RS21, RS22, RS24)

URI.F1 <- URIData %>% select(RS2, RS3, RS4, RS5, RS10, RS13, RS16, RS18, RS19, RS20)

URI.F2 <- URIData %>% select(RS7, RS8, RS9, RS14, RS17, RS21, RS22, RS24)

#Reliability

omega(MTurk.Full, plot = F)
#0.95

omega(MTurk.F1, plot = F)
#0.91

omega(MTurk.F2, plot = F)
#0.93

omega(URI.Full, plot = F)
#0.92

omega(URI.F1, plot = F)
#0.87

omega(URI.F2, plot = F)
#0.89