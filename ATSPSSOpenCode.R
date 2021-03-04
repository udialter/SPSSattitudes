#' ---
#' title: "Validation Code for Attitudes toward SPSS (ATSPSS)"
#' author: "Alter, Dang, Kunicki, Counsell"
#' date: "Jan 31, 2021"
#' output: github_document
#' ---

#' ## Setup
# Packages ----------------------------------------------------------------
#+ message = FALSE, warning = FALSE
library(tidyverse)
library(car)
library(psych)
library(mirt) # for multiple IRT
library(GPArotation) # for Promax rotation
library(MBESS) # for reliability CI
library(REdaS) # for assumptions
library(faoutlier) # for efa outliers
library(mice) # for imputations

# Initial Data Setup ------------------------------------------------------
#' ## Initial Data Setup
#' __Uploading raw data__
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

# Descriptive Stats -------------------------------------------------------
#' ## Descriptive Statistics
describe(spss.data)

#' __Contingency table of the counts__
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

#' __Missing Data Calculations__
table(is.na(spss.data))

# MISSING / (MISSING + NOT MISSING) = PERCENT MISSING 
# 3 / (3 + 1807) = 0.001657459
# 0.001657459 * 100 = 0.1657459

# Less than 1% missing data, proceeding with complete case analyses

#' __Scatterplot Matrix__
car::scatterplotMatrix(spss.data, smooth = F, regLine = F, col = 'black')

# Statistical Assumptions -------------------------------------------------
#' ## Statistical Assumptions
#' __Multivariate Normality__
mardia(spss.data) # Kurtosis = 15.17 >4. Will not assume mvn.

#' __EFA Appropriateness__
# Barlett's Test of Sphericity tests whether a matrix is significantly different from an identity matrix
bart_spher(spss.data, use = "complete.obs") # p-value < 2.22e-16

#' __Kaiser-Meyer-Olkin Statistics__
KMOS(spss.data, use = "complete.obs")
# # KMO-Criterion: 0.8795382

# Listwise Deletion / Complete Case Analysis ------------------------------
#' ## Listwise Deletion / Complete Case Analysis
spss.data.withNA <- spss.data # spss data without removing missing values  

# Previous work suggests using listwise deletion when the missing data rates are extremely low (e.g., < 1%; Flora, 2018; Jakobsen et al., 2017).
spss.data <- spss.data[-c(33, 141, 104), ]
full.data <- full.data[-c(33, 141, 104), ] # needed later for convergent/discriminant validity

spss.data <- data.frame(spss.data)
str(spss.data)

# Polychoric Correlations -------------------------------------------------
#' ## Polychoric Correlations
#' Using polychoric correlations because the data is categorical (5-point Likert scale)
poly.spss.data <- psych::polychoric(spss.data)

# Confidence Intervals for Polychoric Correlations
poly.spss.ci <- (cor.ci(spss.data, poly = TRUE, plot = FALSE))$ci

#' All questions are more correlated w each other than they are with 2,3,10.  
#' But 2,3, and 10 are more correlated to each other than other Qs.  

# Polychoric correlations 
#         SPSS1E SPSS2 SPSS3 SPSS4 SPSS5 SPSS6 SPSS7 SPSS8 SPSS9 SPSS10
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
#           1     2      3    4
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

#                 lower       low.e     upper      up.e            p
# SPSS1E-SPSS2   0.16887050  0.17664235 0.5434718 0.5312478 9.079548e-04
# SPSS1E-SPSS3   0.17465588  0.21318482 0.5146427 0.5181914 3.726149e-04
# SPSS1E-SPSS4   0.78519080  0.78091634 0.9174762 0.9255671 4.072009e-11
# SPSS1E-SPSS5   0.56472304  0.57587917 0.8039876 0.7977725 4.379580e-09
# SPSS1E-SPSS6   0.41084593  0.40785055 0.6723976 0.6717997 8.910949e-09
# SPSS1E-SPSS7   0.44246155  0.45931792 0.6554133 0.6577781 1.556977e-12
# SPSS1E-SPSS8   0.68317581  0.69158350 0.8707006 0.8784873 4.876894e-10
# SPSS1E-SPSS9   0.73608915  0.73238399 0.8808369 0.8776634 1.885159e-13
# SPSS1E-SPSS10  0.06383204  0.07985671 0.4235586 0.4389430 1.079513e-02
# SPSS2-SPSS3    0.57585351  0.55548146 0.7960785 0.7906612 1.773393e-10
# SPSS2-SPSS4    0.14048457  0.16054689 0.4682354 0.4556984 7.909665e-04
# SPSS2-SPSS5    0.14592076  0.17132779 0.5078962 0.5290858 1.272468e-03
# SPSS2-SPSS6    0.09761319  0.10129435 0.4236599 0.4183750 2.986255e-03
# SPSS2-SPSS7    0.01264158  0.02486948 0.4094863 0.3987975 4.102340e-02
# SPSS2-SPSS8    0.11990957  0.14378381 0.5035122 0.4973697 3.293996e-03
# SPSS2-SPSS9    0.11236474  0.11084863 0.4642527 0.4523479 2.696680e-03
# SPSS2-SPSS10   0.31105591  0.34478329 0.6759828 0.6747772 5.113361e-05
# SPSS3-SPSS4    0.21587520  0.22649026 0.5014242 0.5046908 1.438261e-05
# SPSS3-SPSS5    0.08900275  0.09603616 0.4184127 0.4261973 4.066957e-03
# SPSS3-SPSS6    0.04225771  0.04150634 0.3988093 0.4030414 1.857167e-02
# SPSS3-SPSS7   -0.07098063 -0.03761515 0.3330107 0.3504434 1.991385e-01
# SPSS3-SPSS8    0.16206539  0.19877498 0.5154120 0.5178433 7.093637e-04
# SPSS3-SPSS9    0.19042942  0.20562359 0.4913661 0.4687905 7.048877e-05
# SPSS3-SPSS10   0.33988689  0.35322563 0.6533612 0.6393377 2.445603e-06
# SPSS4-SPSS5    0.59466093  0.58378680 0.8304520 0.8317860 1.198729e-08
# SPSS4-SPSS6    0.42805579  0.42679598 0.6970149 0.6923697 2.034349e-08
# SPSS4-SPSS7    0.44822513  0.45353566 0.6691855 0.6655332 8.858914e-12
# SPSS4-SPSS8    0.65588302  0.66229602 0.8362994 0.8339377 1.874279e-12
# SPSS4-SPSS9    0.78614000  0.78944582 0.9229891 0.9204481 4.628460e-10
# SPSS4-SPSS10   0.01931504  0.05856965 0.4017179 0.4126999 3.472357e-02
# SPSS5-SPSS6    0.43081303  0.42401965 0.6913717 0.7007675 7.068568e-09
# SPSS5-SPSS7    0.41485564  0.43249621 0.6649197 0.6754108 1.848346e-09
# SPSS5-SPSS8    0.60905393  0.62834793 0.8279978 0.8207409 1.105110e-09
# SPSS5-SPSS9    0.61242858  0.60744953 0.7971656 0.7887236 1.028067e-13
# SPSS5-SPSS10  -0.08318735 -0.06454500 0.3013012 0.2901654 2.600600e-01
# SPSS6-SPSS7    0.30281013  0.30412899 0.5904182 0.5919838 8.873773e-07
# SPSS6-SPSS8    0.48704107  0.48498981 0.7247681 0.7270032 2.888128e-10
# SPSS6-SPSS9    0.52158114  0.53824960 0.7213030 0.7173948 8.437695e-14
# SPSS6-SPSS10  -0.13411414 -0.13011765 0.2524770 0.2393111 5.396638e-01
# SPSS7-SPSS8    0.47833867  0.50498602 0.7030444 0.6960437 2.075162e-11
# SPSS7-SPSS9    0.44284869  0.45441990 0.6921276 0.6886743 1.426607e-09
# SPSS7-SPSS10  -0.16613093 -0.17224627 0.2096711 0.1825448 8.161641e-01
# SPSS8-SPSS9    0.70160731  0.72195955 0.8761606 0.8784479 1.013167e-10
# SPSS8-SPSS10   0.03870663  0.06280274 0.4227541 0.4189204 2.244871e-02
# SPSS9-SPSS10  -0.09202409 -0.07409066 0.3108612 0.3258500 2.797046e-01



# MAP Test/Parallel Analysis ----------------------------------------------
#' ## MAP Test & Parallel Analysis (PA)
#' __MAP Test__
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

#' __Parallel Analysis__
fa.parallel(spss.data, fm = 'minres', cor = 'poly', fa ='both', n.iter=100)

# Parallel analysis suggests that the number of factors =  2  and the number of components =  2

#' Both MAP and PA suggest 2F. PA should be interpreted w caution for polychoric correlations  
#' Next, running 1F, 2F and 3F model. i.e., 1 above and 1 below suggested num. of factors to help determine which model fits best


# EFA 1 Factor ------------------------------------------------------------
#' ## EFA 1 Factor
efa1 <- fa(r = spss.data, fm = 'minres', rotate = "oblimin", cor = 'poly', nfactors = 1)

#' __EFA 1 Results__
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

#' __EFA Interpretation__  
#' * RMSR = 0.12 = BAD  
#' * Prop. var explained = 0.51   
#' * SPSS10E = BAD factor loading (<.4) and communality (0.07)  
#' * SPSS2E and SPSS3E factor loading almost <.4 and communality almost <.2  
#' * 1 factor model is BAD  
#'
#' __EFA Outliers Check__  
# fS1 <- forward.search(spss.data, 1, criteria = c("mah", "GOF"))
# gcdresult1 <- gCD(spss.data, 1)
# ldresults1 <- LD(spss.data, 1)
#
# plot(gcdresult1)
# plot(fS1)
# plot(ldresults1)

# EFA 2 Factors -----------------------------------------------------------
#' ## EFA 2 Factors
efa2 <- fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 2)

#' __EFA 2 Results__
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

#' __EFA 2 Interpretation__  
#' * RMSR = 0.03 = WOW! huge decrease by adding just 1 more factor  
#' * Prop. var explained = 0.65, 14% raw difference from 1F model  
#' * No poor factor loadings or low communalities  
#' * Column and row parsimony is pretty amazing  
#' * Notice that all negatively worded items load onto factor 2 & all positively worded items load onto factor 1  
#' * 2F prob wins, but let's try 3F next anyways  
#'
#' __EFA 2 Outliers Check__  
# fS2 <- forward.search(spss.data, 2, criteria = c("mah", "GOF"))
# gcdresult2 <- gCD(spss.data, 2)
# ldresults2 <- LD(spss.data, 2)
# 
# plot(gcdresult2)
# plot(fS2)
# plot(ldresults2)

# EFA 3 Factors -----------------------------------------------------------
#' ## EFA 3 Factors
efa3 <- fa(r = spss.data, fm = 'minres', cor = 'poly', nfactors = 3)

#' __EFA 3 Results__
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

#' __EFA 3 Interpretation__  
#' * RMSR = 0.02 = MEH only decreased by 0.01 after adding an additional factor - not worth it bc RMSR always decreases when adding an additional factor.  
#' * Prop. var explained = 0.69, 4% raw difference from 2F model  
#' * No low communalities, BUT  
#' * In general, column and row parsimony is not nearly as good as 2F model  
#' * Concluding that 2F wins bc improvements in model fit isn't worth it. Additionally, column and row parsimony for 3F is worse than 2F model  
#' 
#' __EFA 3 Outliers Check__  
# fS3 <- forward.search(spss.data, 3, criteria = c("mah", "GOF"))
# gcdresult3 <- gCD(spss.data, 3)
# ldresults3 <- LD(spss.data, 3)
# 
# plot(gcdresult3)
# plot(fS3)
# plot(ldresults3)

# Likelihood Ratio Test for EFA 2 vs. EFA 3 -------------------------------
#' ## Likelihood Ratio Test (LRT) for EFA 2 vs. EFA 3  

lrt <- anova(efa2, efa3)

#' __LRT Results__  
# ANOVA Test for Difference Between Models
# 
# df d.df chiSq d.chiSq PR test empirical d.empirical test.echi    BIC d.BIC
# 1 26      87.00                     16.17                       -47.72      
# 2 18    8 53.87   33.13  0 4.14      6.59        9.59       1.2 -39.40  8.32

#' __LRT Interpretation__  
# Lower BIC indicates better fit, therefore, model 1 (i.e., 2-factor EFA) has a better fit than model 2 (3-factor EFA)  

#' For several reasons (e.g., quantitative, interpretabilty, etc.), the 2 Factor model fits the data better than  
#' a 3 factor model. Next, we will try various oblique rotations on the 2 Factor model to get the best row and  
#' column parsimony.  

# Bifactor Model Attempt --------------------------------------------------
#' ## Bifactor Model (unidentified)  
bf_attempt <- psych::schmid(model = poly.spss.data$rho, nfactors = 2, fm = 'minres', rotate = 'oblimin', na.obs = NA, option = 'equal')

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

#' As indicated by the warning, at least three specific factors required for identification (in an EFA context).  
#' As a result, the 2 Factor model still fits the data best.  

# Reliability -------------------------------------------------------------
#' ## Reliability Analyses  
#' Since the 2 Factor model is multidimensional, we calculated reliability for each factor.  
#' We use psych::omega() as recommended by Flora (2020)  
#' 
#' ### __Reliability Calculated Per Factor__

# split dataset into each factor
spss.data.f1 <- spss.data %>% select(-c(SPSS2E, SPSS3E, SPSS10E)) 
spss.data.f2 <- spss.data %>% select(c(SPSS2E, SPSS3E, SPSS10E)) 

#' #### __Using psych::omega()__
# poly = TRUE because we want to use the polychoric correlation matrix instead of Pearson because of our categorical data
# since nfactors = 1, only omega total is meaningful 
omega(m = spss.data.f1, poly = TRUE, plot = F, nfactors = 1) # Omega Total 0.93   
omega(m = spss.data.f2, poly = TRUE, plot = F, nfactors = 1) # Omega Total 0.8   
# warning message regarding 'non-finite result is doubtful' refers to the NA or NaN values in the output. They should not be trusted, but exist because the input provided has NA values

#' #### __Using MBESS:ci.reliability() for 95% CI__
#' It'd be nice to provide 95% CI around omega estimate, so we use the MBESS:ci.reliability() function  
#' The code below follows Flora (2020), but it runs infinitely...  
# ci.reliability(spss.data.f1, type="categorical", interval.type="perc")  
# ci.reliability(spss.data.f2, type="categorical", interval.type="perc")  

#' Changed the interval.type to = "bca" because the ci.reliability() documentation recommends it for categorical omega, but it also runs infinitely...
# ci.reliability(spss.data.f1, type="categorical", interval.type="bca")  
# ci.reliability(spss.data.f2, type="categorical", interval.type="bca")  

#' The code below runs, but does not account for the categorical nature of the items - therefore possibly inappropriate estimate of the scale's reliability
ci.reliability(spss.data.f1) # est 0.9077046, ci.lower 0.8816747, ci.upper 0.9337345  
ci.reliability(spss.data.f2) # est 0.7429931, ci.lower 0.6599966, ci.upper 0.8259896  

#' Overall, we think that MBESS::ci.reliability will not be appropriate here so psych::omega() is preferred
#' 
#' ### __Reliability Calculated for Overall Scale__  
#' The following code calculates omega for the scale overall (i.e., treating it as unidimensional).  
#' This is often requested/reported, but note that this is not appropriate for multi-dimensional models (i.e., our 2F model)
#' 
#' #### __Using psych::omega()__  
omega(m = spss.data, poly = TRUE, plot = F, nfactors = 2) # Omega Total for total scores = 0.93, for F1 = 0.94 and for F2 = 0.80   
#'
#' #### __Using MBESS:ci.reliability() for 95% CI__  
# ci.reliability(spss.data, type="categorical", interval.type="perc") # again, runs infinitely...
# ci.reliability(spss.data, type="categorical", interval.type="bca") # also runs infinitely...

#' The following code runs, but it is not appropriate because it does not account for categorical nature of items.  
# ci.reliability(spss.data) # est = 0.8677201, ci.lower = 0.8353075, ci.upper = 0.9001326

# Convergent Validity -----------------------------------------------------
#' ## Convergent Validity Testing with Quantitative Attitudes Scale  
#' The following section runs Pearson Correlations between ATSPSS scale and Quantitative Attitudes scale  
#' to test for convergent validity.  
#' First, the Quantitative Attitudes scale needs to be setup properly (e.g., reverse code items).  

#' __Quantitative Attitudes Setup__  
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

#' __ATSPSS Setup__  
#' Similarly, the ATSPSS needs to be setup by splitting into its two factors  

# SPSS Attitudes Factor 1
sa.data.f1 <- spss.data.f1 %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# SPSS Attitudes Factor 2
sa.data.f2 <- spss.data.f2 %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)

#' Ok, now we're ready to run the correlations (and scatterplots) for convergent validity testing.  

#' __Correlations__  
cor.test(qa.data.f1$total, sa.data.f1$total) # r = 0.1528329 95% [0.005880347 0.293323814] t = 2.0517, df = 176, p-value = 0.04168  
cor.test(qa.data.f1$total, sa.data.f2$total) # r = 0.1854679 95% [0.03945967 0.32372166] t = 2.504, df = 176, p-value = 0.01319  
car::scatterplot(qa.data.f1$total, sa.data.f1$total)
car::scatterplot(qa.data.f1$total, sa.data.f2$total)

cor.test(qa.data.f2$total, sa.data.f1$total) # r = 0.247044 95% [0.1037284 0.3803096] t = 3.3822, df = 176, p-value = 0.000886  
cor.test(qa.data.f2$total, sa.data.f2$total) # r = 0.1250142 95% [-0.02248388  0.26718600] t = 1.6716, df = 176, p-value = 0.09638  
car::scatterplot(qa.data.f2$total, sa.data.f1$total)
car::scatterplot(qa.data.f2$total, sa.data.f2$total)

#' The following section runs Pearson Correlations between ATSPSS scale  
#' and Quantitative Anxiety scale / Quantitative Hindrances scale  

#' __Quantitative Anxiety__
# Set up Quant. Anxiety scale by selecting only the items and calculating total score
qanx.data <- full.data %>% select(
  QANX1E:QANX4E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)

# Ok, now for the correlations  
cor.test(qanx.data$total, sa.data.f1$total) # r = -0.06134227 95% [-0.20656324  0.08652308] t = -0.81533, df = 176, p-value = 0.416  
cor.test(qanx.data$total, sa.data.f2$total) # r = -0.07568429 95% [-0.22031643  0.07220418] t = -1.007, df = 176, p-value = 0.3153  
car::scatterplot(qanx.data$total, sa.data.f1$total)
car::scatterplot(qanx.data$total, sa.data.f2$total)

#' __Quantitative Hindrances__  
#' Again, first set up Quant. Hindrances scale by selecting only the items and calculating total score  
qh.data <- full.data %>% select(
  QHIND1E:QHIND5E
) %>% mutate(
  total = rowSums(.[1:ncol(.)], na.rm = TRUE)
)
# Now for the correlations  
cor.test(qh.data$total, sa.data.f1$total) # r = -0.03837761 95% [-0.1844213  0.1093242] t = -0.50951, df = 176, p-value = 0.611  
cor.test(qh.data$total, sa.data.f2$total) # r = -0.06702523 95% [-0.21201975  0.08085658] t = -0.89119, df = 176, p-value = 0.374  
car::scatterplot(qh.data$total, sa.data.f1$total)
car::scatterplot(qh.data$total, sa.data.f2$total)

# Correlations between all Measures ---------------------------------------

#' ## Remaining Correlations between All Measures
#' The following correlations are to fill out the remaining cells of the convergent/discriminant validity table.  
#' 
#' __ATSPSS Factor 1 x ATSPSS Factor 2__
cor.test(sa.data.f1$total, sa.data.f2$total) # r = 0.2943086 95% [0.1538854 0.4230790]  t = 4.0854, df = 176, p-value = 6.675e-05

#' __Quant. Attitudes Factor 1 x Quant. Attitudes Factor 2__
cor.test(qa.data.f1$total, qa.data.f2$total) # r = 0.4117248 95% [0.2816993 0.5269012] t = 5.9937, df = 176, p-value = 1.132e-08

#' __Quant. Attitudes Factor 1 x Quant. Anxiety__
cor.test(qa.data.f1$total, qanx.data$total) # r = -0.5866839 95% [ -0.6754799 -0.4811158] t = -9.6111, df = 176, p-value < 2.2e-16

#' __Quant. Attitudes Factor 1 x Quant. Hindrances__
cor.test(qa.data.f1$total, qh.data$total) # r = -0.4092008 95% [-0.5247049 -0.2789024] t = -5.9496, df = 176, p-value = 1.418e-08

#' __Quant. Attitudes Factor 2 x Quant. Anxiety__
cor.test(qa.data.f2$total, qanx.data$total) # r = -0.3704627 95% [-0.4908038 -0.2362512] t = -5.2912, df = 176, p-value = 3.58e-07

#' __Quant. Attitudes Factor 2 x Quant. Hindrances__
cor.test(qa.data.f2$total, qh.data$total) # r = -0.2618719 95% [-0.3937889 -0.1193856] t = -3.5997, df = 176, p-value = 0.000414

#' __Quant. Anxiety x Quant. Hindrances__
cor.test(qanx.data$total, qh.data$total) # r = 0.6228247 95% [0.5237166 0.7052984] t = 10.561, df = 176, p-value < 2.2e-16

# Confirmatory Multidimensional Item Response Theory (MIRT) ---------------
#' ## Confirmatory Multidimensional Item Response Theory (MIRT)

irtmodel <- " 
F1 = SPSS1E, SPSS4E, SPSS5E, SPSS6E, SPSS7E, SPSS8E, SPSS9E
F2 = SPSS2E, SPSS3E, SPSS10E
COV = F1*F2" #asterisk used to call for covariance

cmirtmod <- mirt.model(irtmodel, itemnames = spss.data) 

cmirt <- mirt(data = spss.data, model = cmirtmod, itemtype = 'graded')

coef(cmirt , IRTParam = T, simplify = T)

#' __MIRT Results__
# $items
#         a1    a2    d1    d2     d3     d4
# SPSS1E  3.997 0.000 6.957 4.064  0.530 -4.052
# SPSS2E  0.000 3.086 6.079 3.087  1.447 -2.197
# SPSS3E  0.000 3.066 5.328 3.155  0.960 -3.106
# SPSS4E  4.467 0.000 8.295 3.983  0.186 -5.773
# SPSS5E  2.774 0.000 5.857 3.631  0.793 -1.996
# SPSS6E  1.639 0.000 3.135 2.038  0.207 -1.466
# SPSS7E  1.588 0.000 3.655 1.407 -0.080 -1.945
# SPSS8E  3.442 0.000 7.028 4.088  0.684 -3.426
# SPSS9E  4.560 0.000 8.014 4.228 -0.078 -4.656
# SPSS10E 0.000 1.573 3.648 1.811  0.212 -2.034
# 
# $means
# F1 F2 
# 0  0 
# 
# $cov
# F1  F2
# F1 1.0 0.5
# F2 0.5 1.0

#' __MIRT Model Fit__
M2(cmirt, type = "C2")
#       M2        df           p      RMSEA   RMSEA_5   RMSEA_95      SRMSR       TLI       CFI
# stats 57.45641 34 0.007196968 0.06243163 0.0324369 0.08944299 0.08820926 0.9811121 0.9857292

#' __MIRT Assumptions__
residuals(cmirt)
# LD matrix (lower triangle) and standardized values:
#   
#           SPSS1E  SPSS2E SPSS3E  SPSS4E  SPSS5E SPSS6E SPSS7E  SPSS8E  SPSS9E SPSS10E
# SPSS1E       NA   0.424  0.392   0.226  -0.461 -0.199 -0.224  -0.284  -0.249   0.397
# SPSS2E  127.880      NA -0.424  -0.427   0.264 -0.214 -0.289  -0.323  -0.363  -0.380
# SPSS3E  109.210 128.186     NA  -0.324  -0.279 -0.212 -0.258   0.272  -0.257  -0.350
# SPSS4E   36.465 129.587 74.758      NA  -0.806 -0.196 -0.186  -0.309  -0.292  -0.401
# SPSS5E  151.375  49.721 55.339 462.624      NA  0.271 -0.218  -0.252  -0.197  -0.413
# SPSS6E   28.309  32.688 31.957  27.333  52.439     NA -0.264   0.166  -0.184  -0.302
# SPSS7E   35.818  59.299 47.315  24.502  33.901 49.514     NA  -0.237  -0.244  -0.352
# SPSS8E   57.491  74.379 52.782  67.942  45.266 19.632 40.000      NA  -0.258   0.452
# SPSS9E   44.141  93.740 46.854  60.503  27.716 24.233 42.376  47.429      NA  -0.479
# SPSS10E 112.082 103.058 87.417 114.234 121.545 64.857 87.976 145.586 163.213      NA

# Sensitivity Analyses ----------------------------------------------------
#' ## Sensitivity Analyses

#' ### __Setting up Datasets__

# missing data is removed using listwise deletion
str(spss.data)
# missing data is not removed. This data will be used to imputing missing values later.
str(spss.data.withNA)

#' ### __Assumptions for Maximum Likelihood__ 
#' Refer to 'Statistical Assumptions' section at the beginning
# Multivariate Normality
# Linearity

#' ### __Treating our 5-Point Likert Scale as Continuous instead of Categorical__

#' #### __First, let's do this with the 2 factor EFA model__

#' ##### __Using Pearson correlations, ML estimator and listwise deletion__
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

#' #### __OK, now let's do the same thing, but with the 3 factor EFA model__

#' ##### __Using Pearson correlations, ML estimator and listwise deletion__
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

#' ### __Setup: Imputations for Missing Values__
imp <- mice(spss.data.withNA, m = 20) 
imp <-  complete(imp)
sum(is.na(imp)) # double checking that there is no missing data

#' ### __Treating Our Data as Categorical, but imputed missing values__
#' #### __First, with the 2 factor EFA model__  
fa(r = imp, fm = "minres", cor = 'poly', nfactors = 2)

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

#' #### __Next, with the 3 factor EFA model__  
fa(r = imp, fm = "minres", cor = 'poly', nfactors = 3)

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

#' ### __Treating Our Data as Categorical, but using pairwise deletion for missing values__

#' #### __First, with the 2 factor EFA model__  
fa(r = spss.data.withNA, use = "pairwise", fm = "minres", cor = 'poly', nfactors = 2)

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

#' #### __Next, with the 3 factor EFA model__  
fa(r = spss.data.withNA, use = "pairwise", fm = "minres", cor = 'poly', nfactors = 3)

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

#' ### __Oblique Rotations for 2 Factor EFA__
#' Since we decided the 2 factor EFA fit the data best, we're going to try various oblique rotations  
#' to find the one that provides the best row and column parsimony:  

#' __BentlerQ__
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

#' __GeominQ__
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

#' __Quartimin__
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

#' __Promax__
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

#' Overall, none of the rotations were significantly better than the default oblimin rotation.  
#' So, we decided to just stick with oblimin.  
